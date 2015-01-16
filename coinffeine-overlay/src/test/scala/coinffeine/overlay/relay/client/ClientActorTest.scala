package coinffeine.overlay.relay.client

import java.net.InetSocketAddress
import scala.concurrent.duration._

import akka.actor.ActorRef
import akka.io.Tcp
import akka.testkit._
import akka.util.ByteString
import org.scalatest.Inside

import coinffeine.common.akka.test.AkkaSpec
import coinffeine.overlay.OverlayNetwork.{JoinFailed, NetworkStatus, UnexpectedLeave}
import coinffeine.overlay.relay.client.RelayNetwork.{HandshakeFailed, InvalidDataReceived}
import coinffeine.overlay.relay.messages._
import coinffeine.overlay.relay.settings.RelayClientSettings
import coinffeine.overlay.{OverlayId, OverlayNetwork}

class ClientActorTest extends AkkaSpec with Inside {

  val clientId = OverlayId(1)
  val otherId = OverlayId(2)
  val sampleMessage = ByteString("hello")
  val config = RelayClientSettings(
    host = "localhost",
    port = 1234,
    connectionTimeout = 300.millis.dilated,
    identificationTimeout = 300.millis.dilated,
    maxFrameBytes = 1024
  )

  "A relay network client actor" should "identify itself upon connection" in new Fixture {
    expectSuccessfulJoin()
  }

  it should "drop the connection if the server doesn't respond" in new Fixture {
    expectSuccessfulConnection()
    tcpProbe.expectMsg(Tcp.Write(ProtobufFrame.serialize(JoinMessage(clientId))))
    expectHandshakeFailure()
  }

  it should "drop the connection if the server respond with invalid data" in new Fixture {
    expectSuccessfulConnection()
    val invalidData = ByteString("not a frame")
    tcpProbe.expectMsg(Tcp.Write(ProtobufFrame.serialize(JoinMessage(clientId))))
    tcpProbe.reply(Tcp.Received(invalidData))
    expectHandshakeFailure()
  }

  it should "drop the connection if the server respond anything but a network status" in new Fixture {
    expectSuccessfulConnection()
    val invalidResponse = JoinMessage(OverlayId(34))
    tcpProbe.expectMsg(Tcp.Write(ProtobufFrame.serialize(JoinMessage(clientId))))
    tcpProbe.reply(Tcp.Received(ProtobufFrame.serialize(invalidResponse)))
    expectHandshakeFailure()
  }

  it should "reject join requests while processing another join" in new Fixture {
    expectConnectionAttemptOnJoin()
    client ! OverlayNetwork.Join(otherId)
    expectMsg(OverlayNetwork.JoinFailed(otherId, OverlayNetwork.AlreadyJoining))
  }

  it should "reject join requests when already joined" in new Fixture {
    expectSuccessfulJoin()
    client ! OverlayNetwork.Join(otherId)
    expectMsg(OverlayNetwork.JoinFailed(otherId, OverlayNetwork.AlreadyJoined))
  }

  it should "notify connection failure" in new Fixture {
    expectConnectionAttemptOnJoin()
    tcpProbe.send(client, Tcp.CommandFailed(Tcp.Connect(new InetSocketAddress("localhost", 1234))))
    inside(expectMsgType[OverlayNetwork.JoinFailed]) {
      case OverlayNetwork.JoinFailed(`clientId`, OverlayNetwork.UnderlyingNetworkFailure(_)) =>
    }
  }

  it should "notify server resolution failure" in {
    val invalidHost = "does.not.exist.example.com"
    val client = system.actorOf(ClientActor.props(config.copy(host = invalidHost), ActorRef.noSender))
    client ! OverlayNetwork.Join(clientId)
    inside (expectMsgType[OverlayNetwork.JoinFailed].cause) {
      case OverlayNetwork.UnderlyingNetworkFailure(ex) =>
        ex.getMessage should include (s"Cannot resolve $invalidHost")
    }
  }

  it should "close the connection when asked to leave" in new Fixture {
    expectSuccessfulJoin()
    expectConnectionCloseOnLeave()
    tcpProbe.reply(Tcp.Closed)
    expectMsg(OverlayNetwork.Leaved(clientId, OverlayNetwork.RequestedLeave))
  }

  it should "reject join requests while closing" in new Fixture {
    expectSuccessfulJoin()
    expectConnectionCloseOnLeave()
    client ! OverlayNetwork.Join(otherId)
    expectMsg(OverlayNetwork.JoinFailed(otherId, OverlayNetwork.AlreadyJoined))
  }

  it should "notify the network status" in new Fixture {
    expectSuccessfulJoin()

    tcpProbe.send(client, Tcp.Received(ProtobufFrame.serialize(StatusMessage(2))))
    expectMsg(OverlayNetwork.NetworkStatus(2))
  }

  it should "become disconnected if the connection is interrupted" in new Fixture {
    expectSuccessfulJoin()
    expectLeavingAfterAnUnexpectedConnectionClose()
  }

  it should "try to connect after a disconnection if asked to" in new Fixture {
    expectSuccessfulJoin()
    expectLeavingAfterAnUnexpectedConnectionClose()
    expectSuccessfulJoin()
  }

  it should "relay messages to the network" in new Fixture {
    expectSuccessfulJoin()
    client ! OverlayNetwork.SendMessage(otherId, sampleMessage)
    tcpProbe.expectMsg(Tcp.Write(ProtobufFrame.serialize(RelayMessage(otherId, sampleMessage))))
  }

  it should "not relay messages when disconnected from the network" in new Fixture {
    expectUnavailableNetworkForSendingMessages()
  }

  it should "not relay messages when connecting to the network" in new Fixture {
    expectConnectionAttemptOnJoin()
    expectUnavailableNetworkForSendingMessages()
  }

  it should "not relay messages when disconnecting from the network" in new Fixture {
    expectSuccessfulJoin()
    expectConnectionCloseOnLeave()
    expectUnavailableNetworkForSendingMessages()
  }

  it should "not relay messages exceeding the max frame bytes" in new Fixture {
    expectSuccessfulJoin()
    val tooLargePayload = Array.fill[Byte](config.maxFrameBytes - Frame.HeaderSize + 1)(0)
    val tooLargeMessage = ProtobufFrame.serialize(RelayMessage(otherId, ByteString(tooLargePayload)))
    client ! OverlayNetwork.SendMessage(otherId, tooLargeMessage)
    expectMsgType[OverlayNetwork.CannotSend]
  }

  it should "receive messages from the network" in new Fixture {
    expectSuccessfulJoin()

    tcpProbe.send(client, Tcp.Received(ProtobufFrame.serialize(RelayMessage(otherId, sampleMessage))))
    expectMsg(OverlayNetwork.ReceiveMessage(otherId, sampleMessage))
  }

  it should "drop the connection if a join message is received" in new Fixture {
    expectSuccessfulJoin()
    val invalidMessage = ProtobufFrame.serialize(JoinMessage(otherId))
    expectConnectionDroppedOnInvalidMessage(invalidMessage, "Unexpected message received")
  }

  it should "drop the connection if an invalid protobuf is received" in new Fixture {
    expectSuccessfulJoin()
    val notAProtobuf = Frame(ByteString(1, 1, 2, 3, 5, 8)).serialize
    expectConnectionDroppedOnInvalidMessage(notAProtobuf, "Cannot parse protobuf")
  }

  it should "drop the connection if an invalid frame is received" in new Fixture {
    expectSuccessfulJoin()
    val invalidFrame = ByteString(42, 3, 5)
    expectConnectionDroppedOnInvalidMessage(invalidFrame, "Cannot delimit frame: Bad magic number")
  }

  it should "properly delimit messages from the network" in new Fixture {
    expectSuccessfulJoin()

    val concatenatedMessages = Seq("oh", "my", "god!").flatMap { payload =>
      ProtobufFrame.serialize(RelayMessage(otherId, ByteString(payload)))
    }.toArray
    concatenatedMessages.grouped(10).foreach { chunk =>
      tcpProbe.send(client, Tcp.Received(ByteString(chunk)))
    }

    expectMsgAllOf(
      OverlayNetwork.ReceiveMessage(otherId, ByteString("oh")),
      OverlayNetwork.ReceiveMessage(otherId, ByteString("my")),
      OverlayNetwork.ReceiveMessage(otherId, ByteString("god!"))
    )
  }

  trait Fixture {
    protected val tcpProbe = TestProbe()
    protected val client = system.actorOf(ClientActor.props(config, tcpProbe.ref))

    def expectConnectionAttemptOnJoin(): Unit = {
      client ! OverlayNetwork.Join(clientId)
      tcpProbe.expectMsg(Tcp.Connect(
        remoteAddress = new InetSocketAddress("localhost", 1234),
        timeout = Some(config.connectionTimeout)
      ))
    }

    def expectSuccessfulConnection(): Unit = {
      expectConnectionAttemptOnJoin()
      tcpProbe.reply(Tcp.Connected(
        new InetSocketAddress("localhost", 1234), new InetSocketAddress("localhost", 18734)))
      tcpProbe.expectMsg(Tcp.Register(client))
    }

    def expectSuccessfulJoin(): Unit = {
      expectSuccessfulConnection()

      tcpProbe.expectMsg(Tcp.Write(ProtobufFrame.serialize(JoinMessage(clientId))))
      tcpProbe.reply(Tcp.Received(ProtobufFrame.serialize(StatusMessage(1))))

      expectMsg(OverlayNetwork.Joined(clientId, NetworkStatus(1)))
    }

    def expectLeavingAfterAnUnexpectedConnectionClose(): Unit = {
      tcpProbe.send(client, Tcp.Closed)
      inside (expectMsgType[OverlayNetwork.Leaved]) {
        case OverlayNetwork.Leaved(`clientId`, UnexpectedLeave(_)) =>
      }
    }

    def expectConnectionCloseOnLeave(): Unit = {
      client ! OverlayNetwork.Leave
      tcpProbe.expectMsg(Tcp.Close)
    }

    def expectUnavailableNetworkForSendingMessages(): Unit = {
      val request = OverlayNetwork.SendMessage(otherId, sampleMessage)
      client ! request
      expectMsg(OverlayNetwork.CannotSend(request, OverlayNetwork.UnavailableNetwork))
    }

    def expectConnectionDroppedOnInvalidMessage(invalidMessage: ByteString, expectedError: String): Unit = {
      tcpProbe.send(client, Tcp.Received(invalidMessage))
      tcpProbe.expectMsg(Tcp.Close)
      tcpProbe.reply(Tcp.Closed)

      inside(expectMsgType[OverlayNetwork.Leaved]) {
        case OverlayNetwork.Leaved(`clientId`, OverlayNetwork.UnexpectedLeave(InvalidDataReceived(message, _))) =>
          message should include(expectedError)
      }
    }

    def expectHandshakeFailure(): Unit = {
      inside(expectMsgType[JoinFailed]) {
        case OverlayNetwork.JoinFailed(`clientId`,
        OverlayNetwork.UnderlyingNetworkFailure(_: HandshakeFailed)) =>
      }
    }
  }
}
