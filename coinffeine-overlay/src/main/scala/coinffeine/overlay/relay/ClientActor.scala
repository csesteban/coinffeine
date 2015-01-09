package coinffeine.overlay.relay

import java.io.IOException
import java.net.{InetAddress, InetSocketAddress}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

import akka.actor._
import akka.io.{IO, Tcp}
import akka.util.ByteString

import coinffeine.overlay.relay.RelayNetwork._
import coinffeine.overlay.relay.messages._
import coinffeine.overlay.{OverlayId, OverlayNetwork}

private[this] class ClientActor(config: ClientConfig, tcpManager: ActorRef)
  extends Actor with Stash with ActorLogging {

  override def receive: Receive = disconnected

  private def disconnected: Receive = notSendingMessages orElse {
    case OverlayNetwork.Join(id) =>
      resolveAddress() match {
        case Success(remoteAddress) =>
          becomeConnecting(id, remoteAddress, sender())

        case Failure(ex) =>
          val cause = new IOException(s"Cannot resolve ${config.host}:${config.port}", ex)
          sender() ! OverlayNetwork.JoinFailed(id, OverlayNetwork.UnderlyingNetworkFailure(cause))
      }
  }

  private def resolveAddress(): Try[InetSocketAddress] =
    Try(InetAddress.getByName(config.host)).map { host =>
      new InetSocketAddress(host, config.port)
    }

  private def becomeConnecting(id: OverlayId, remoteAddress: InetSocketAddress, listener: ActorRef): Unit = {
    tcpManager ! Tcp.Connect(remoteAddress, timeout = Some(config.connectionTimeout))
    context.become(notSendingMessages orElse {
      case OverlayNetwork.Join(otherId) =>
        sender() ! OverlayNetwork.JoinFailed(otherId, OverlayNetwork.AlreadyJoining)

      case Tcp.Connected(_, _) =>
        val connection = sender()
        connection ! Tcp.Register(self)
        connection ! Tcp.Write(ProtobufFrame.serialize(JoinMessage(id)))
        becomeConnected(id, connection, listener)

      case Tcp.CommandFailed(_: Tcp.Connect) =>
        val cause = OverlayNetwork.UnderlyingNetworkFailure(CannotStartConnection(remoteAddress))
        listener ! OverlayNetwork.JoinFailed(id, cause)
        context.become(disconnected)
    })
  }

  private def becomeConnected(id: OverlayId, connection: ActorRef, listener: ActorRef): Unit = {

    var buffer = ByteString.empty

    @tailrec
    def decodeFrames(): Unit = {
      Frame.deserialize(buffer, config.maxFrameBytes) match {
        case Frame.IncompleteInput => // Stop for the moment

        case Frame.Parsed(Frame(protobuf), remainingBuffer) =>
          buffer = remainingBuffer
          Try(ProtobufConversion.fromProtobuf(protobuf)) match {
            case Success(message) =>
              handleMessage(message)
              decodeFrames()
            case Failure(ex) =>
              val cause = InvalidDataReceived("Cannot parse protobuf", ex)
              becomeDisconnecting(id, connection, listener, OverlayNetwork.UnexpectedLeave(cause))
          }

        case Frame.FailedParsing(message) =>
          val cause = InvalidDataReceived(s"Cannot delimit frame: $message")
          becomeDisconnecting(id, connection, listener, OverlayNetwork.UnexpectedLeave(cause))
      }
    }

    def handleMessage(message: Message): Unit = {
      message match {
        case StatusMessage(networkSize) =>
          listener ! OverlayNetwork.NetworkStatus(networkSize)
        case RelayMessage(senderId, payload) =>
          listener ! OverlayNetwork.ReceiveMessage(senderId, payload)
        case joinMessage: JoinMessage =>
          val cause = InvalidDataReceived(s"Unexpected message received: $joinMessage")
          becomeDisconnecting(id, connection, listener, OverlayNetwork.UnexpectedLeave(cause))
      }
    }

    context.become(alreadyJoined orElse {
      case request @ OverlayNetwork.SendMessage(to, message) =>
        val frameBytes = ProtobufFrame.serialize(RelayMessage(to, message))
        if (frameBytes.size > config.maxFrameBytes) {
          val cause = MessageTooLarge(frameBytes.size, config.maxFrameBytes - Frame.HeaderSize)
          listener ! OverlayNetwork.CannotSend(request, OverlayNetwork.UnderlyingNetworkFailure(cause))
        } else {
          connection ! Tcp.Write(frameBytes)
        }

      case Tcp.Received(data) =>
        buffer ++= data
        decodeFrames()

      case closed: Tcp.ConnectionClosed =>
        val cause = OverlayNetwork.UnexpectedLeave(
          UnexpectedConnectionTermination(closed.getErrorCause))
        listener ! OverlayNetwork.Leaved(id, cause)
        context.become(disconnected)

      case OverlayNetwork.Leave =>
        becomeDisconnecting(id, connection, listener, OverlayNetwork.RequestedLeave)
    })
  }

  private def becomeDisconnecting(id: OverlayId,
                                  connection: ActorRef,
                                  listener: ActorRef,
                                  cause: OverlayNetwork.LeaveCause): Unit = {
    connection ! Tcp.Close
    context.become(alreadyJoined orElse notSendingMessages orElse {
      case _: Tcp.ConnectionClosed =>
        listener ! OverlayNetwork.Leaved(id, cause)
        context.become(disconnected)
    })
  }

  private def alreadyJoined: Receive = {
    case OverlayNetwork.Join(otherId) =>
      sender() ! OverlayNetwork.JoinFailed(otherId, OverlayNetwork.AlreadyJoined)
  }

  private def notSendingMessages: Receive = {
    case request: OverlayNetwork.SendMessage =>
      sender() ! OverlayNetwork.CannotSend(request, OverlayNetwork.UnavailableNetwork)
  }
}

private object ClientActor {
  def props(config: ClientConfig)(implicit system: ActorSystem): Props = props(config, IO(Tcp))
  def props(config: ClientConfig, tcpManager: ActorRef): Props =
    Props(new ClientActor(config, tcpManager))
}
