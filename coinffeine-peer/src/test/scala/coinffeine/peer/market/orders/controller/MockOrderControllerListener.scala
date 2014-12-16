package coinffeine.peer.market.orders.controller

import org.scalatest.Assertions

import coinffeine.model.currency.FiatCurrency
import coinffeine.model.market.{OrderStatus, Order}

class MockOrderControllerListener[C <: FiatCurrency]
  extends OrderController.Listener[C] with Assertions {

  var currentOrder: Order[C] = _

  override def onOrderChange(oldOrder: Order[C], newOrder: Order[C]): Unit = {
    currentOrder = newOrder
  }

  def lastStatus: OrderStatus = {
    require(currentOrder != null, "Order status was never updated")
    currentOrder.status
  }

  def inMarket: Boolean = currentOrder.shouldBeOnMarket
}
