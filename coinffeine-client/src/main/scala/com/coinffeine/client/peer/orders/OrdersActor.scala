package com.coinffeine.client.peer.orders

import akka.actor._

import com.coinffeine.client.peer.CoinffeinePeerActor.{CancelOrder, OpenOrder}
import com.coinffeine.common.{Order, FiatCurrency, PeerConnection}
import com.coinffeine.common.protocol.ProtocolConstants
import com.coinffeine.common.protocol.messages.brokerage._

/** Manages open orders */
class OrdersActor(protocolConstants: ProtocolConstants) extends Actor with ActorLogging {

  override def receive: Receive = {
    case init: OrdersActor.Initialize =>
      new InitializedOrdersActor(init).start()
  }

  private class InitializedOrdersActor(init: OrdersActor.Initialize) {
    import init._

    private var delegatesByMarket = Map.empty[Market[FiatCurrency], ActorRef]

    def start(): Unit = {
      context.become(waitingForOrders)
    }

    private val waitingForOrders: Receive = {
      case message @ OpenOrder(order) =>
        getOrCreateDelegate(marketOf(order)) forward message
      case message @ CancelOrder(order) =>
        getOrCreateDelegate(marketOf(order)) forward message
    }

    private def marketOf(order: Order) = Market(currency = order.price.currency)

    private def getOrCreateDelegate(market: Market[FiatCurrency]): ActorRef =
      delegatesByMarket.getOrElse(market, createDelegate(market))

    private def createDelegate(market: Market[FiatCurrency]): ActorRef = {
      log.info(s"Start submitting to $market")
      val newDelegate = context.actorOf(OrderSubmissionActor.props(protocolConstants))
      newDelegate ! OrderSubmissionActor.Initialize(market, gateway, brokerAddress)
      delegatesByMarket += market -> newDelegate
      newDelegate
    }
  }
}

object OrdersActor {

  case class Initialize(ownAddress: PeerConnection, brokerAddress: PeerConnection, gateway: ActorRef)

  trait Component { this: ProtocolConstants.Component =>
    lazy val ordersActorProps = Props(new OrdersActor(protocolConstants))
  }
}
