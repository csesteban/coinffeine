package com.coinffeine.common.protocol.gateway

import scala.concurrent.duration._

import akka.actor.ActorRef
import akka.testkit.{TestActorRef, TestProbe}
import com.googlecode.protobuf.pro.duplex.PeerInfo
import org.scalatest.concurrent.{Eventually, IntegrationPatience}

import com.coinffeine.common.PeerConnection
import com.coinffeine.common.network.CoinffeineUnitTestNetwork
import com.coinffeine.common.protocol.{ProtocolConstants, TestClient}
import com.coinffeine.common.protocol.gateway.MessageGateway.{Bind, BoundTo, ReceiveMessage}
import com.coinffeine.common.protocol.messages.brokerage.OrderMatch
import com.coinffeine.common.protocol.serialization._
import com.coinffeine.common.test.{AkkaSpec, DefaultTcpPortAllocator}

class ProtoRpcMessageGatewayTest extends AkkaSpec("MessageGatewaySystem")
  with Eventually with IntegrationPatience {

  val receiveTimeout = 10.seconds

  "Protobuf RPC Message gateway" must "send a known message to a remote peer" in new FreshGateway {
    val (message, protoMessage) = randomMessageAndSerialization()
    gateway ! MessageGateway.ForwardMessage(message, remotePeerConnection)
    eventually {
      remotePeer.receivedMessagesNumber should be (1)
      remotePeer.receivedMessages contains protoMessage
    }
  }

  it must "send a known message twice reusing the connection to the remote peer" in
    new FreshGateway {
      val (msg1, msg2) = (randomMessage(), randomMessage())
      gateway ! MessageGateway.ForwardMessage(msg1, remotePeerConnection)
      gateway ! MessageGateway.ForwardMessage(msg2, remotePeerConnection)
      eventually {
        remotePeer.receivedMessagesNumber should be (2)
        remotePeer.receivedMessages contains protocolSerialization.toProtobuf(msg1)
        remotePeer.receivedMessages contains protocolSerialization.toProtobuf(msg2)
      }
    }

  it must "throw while forwarding when recipient was never connected" in new FreshGateway {
    val msg = randomMessage()
    remotePeer.shutdown()
    a [MessageGateway.ForwardException] should be thrownBy {
      testGateway.receive(MessageGateway.ForwardMessage(msg, remotePeerConnection))
    }
  }

  val subscribeToOrderMatches = MessageGateway.Subscribe {
    case ReceiveMessage(msg: OrderMatch, _) => true
    case _ => false
  }

  it must "deliver messages to subscribers when filter match" in new FreshGateway {
    val msg = randomMessage()
    gateway ! subscribeToOrderMatches
    remotePeer.sendMessage(msg)
    expectMsg(receiveTimeout, ReceiveMessage(msg, remotePeer.connection))
  }

  it must "do not deliver messages to subscribers when filter doesn't match" in new FreshGateway {
    val msg = randomMessage()
    gateway ! MessageGateway.Subscribe(msg => false)
    remotePeer.sendMessage(msg)
    expectNoMsg()
  }

  it must "deliver messages to several subscribers when filter match" in new FreshGateway {
    val msg = randomMessage()
    val subs = for (i <- 1 to 5) yield TestProbe()
    subs.foreach(_.send(gateway, subscribeToOrderMatches))
    remotePeer.sendMessage(msg)
    subs.foreach(_.expectMsg(receiveTimeout, ReceiveMessage(msg, remotePeer.connection)))
  }

  trait FreshGateway extends ProtoRpcMessageGateway.Component
      with TestProtocolSerializationComponent with CoinffeineUnitTestNetwork.Component
      with ProtocolConstants.DefaultComponent {
    val (localPeerAddress, gateway) = createGateway()
    val (remotePeerConnection, remotePeer) = createRemotePeer(localPeerAddress)
    val testGateway = createGatewayTestActor

    private def createGateway(): (PeerConnection, ActorRef) = {
      val peerConnection = allocateLocalPeerConnection()
      val ref = system.actorOf(messageGatewayProps)
      eventually {
        ref ! Bind(peerConnection)
        expectMsg(BoundTo(peerConnection))
      }
      (peerConnection, ref)
    }

    private def createGatewayTestActor: TestActorRef[ProtoRpcMessageGateway] = {
      val peerConnection = allocateLocalPeerConnection()
      val ref = TestActorRef(new ProtoRpcMessageGateway(protocolSerialization))
      eventually {
        ref ! Bind(peerConnection)
        expectMsg(BoundTo(peerConnection))
      }
      ref
    }

    private def createRemotePeer(localPeerAddress: PeerConnection): (PeerConnection, TestClient) = {
      val peerConnection = allocateLocalPeerConnection()
      val localPeerInfo = new PeerInfo(localPeerAddress.hostname, localPeerAddress.port)
      eventually {
        val client = new TestClient(peerConnection.port, localPeerInfo, protocolSerialization)
        client.connectToServer()
        (peerConnection, client)
      }
    }

    private def allocateLocalPeerConnection() =
      PeerConnection("localhost", DefaultTcpPortAllocator.allocatePort())
  }
}
