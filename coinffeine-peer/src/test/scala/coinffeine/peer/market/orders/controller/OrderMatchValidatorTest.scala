package coinffeine.peer.market.orders.controller

import org.scalatest.Inside

import coinffeine.common.test.UnitTest
import coinffeine.model.bitcoin.KeyPair
import coinffeine.model.bitcoin.test.CoinffeineUnitTestNetwork
import coinffeine.model.currency._
import coinffeine.model.exchange._
import coinffeine.model.market._
import coinffeine.model.network.PeerId
import coinffeine.peer.amounts.DefaultAmountsComponent
import coinffeine.protocol.messages.brokerage.OrderMatch

class OrderMatchValidatorTest extends UnitTest with Inside with DefaultAmountsComponent {

  private val ownId = PeerId.random()
  private val validator = new OrderMatchValidator(ownId, amountsCalculator)
  private val order = Order.randomLimit(Bid, 0.9997.BTC, Price(110.263078923677103, Euro))
  private val orderMatch = OrderMatch(
    orderId = order.id,
    exchangeId = ExchangeId.random(),
    bitcoinAmount = Both(0.9997.BTC, 1.BTC),
    fiatAmount = Both(100.EUR, 99.5.EUR),
    lockTime = 37376,
    counterpart = PeerId.random()
  )
  val exchange = Exchange.handshaking[Euro.type](
    id = orderMatch.exchangeId,
    role = BuyerRole,
    counterpartId = orderMatch.counterpart,
    amounts = amountsCalculator.exchangeAmountsFor(orderMatch),
    parameters = Exchange.Parameters(orderMatch.lockTime, network = CoinffeineUnitTestNetwork)
  )

  "An order match validator" should "reject any order match if the order is finished" in {
    expectRejectionWithMessage(order.cancel, orderMatch, "Order already finished")
  }

  it should "reject already accepted matches" in {
    val exchangingOrder = order.start.copy(exchanges = Map(exchange.id -> exchange))
    inside(validator.shouldAcceptOrderMatch(
      exchangingOrder, orderMatch, orderMatch.bitcoinAmount.buyer)) {
        case MatchAlreadyAccepted(_) =>
      }
  }

  it should "reject self-matches" in {
    expectRejectionWithMessage(order, orderMatch.copy(counterpart = ownId), "Self-cross")
  }

  it should "reject matches of more amount than the pending one" in {
    val tooLargeOrderMatch = orderMatch.copy[Euro.type](bitcoinAmount = Both(1.9997.BTC, 2.BTC))
    expectRejectionWithMessage(order, tooLargeOrderMatch, "Invalid amount")
  }

  it should "reject matches whose price is off limit when buying" in {
    val tooHighPriceMatch = orderMatch.copy(fiatAmount = Both(120.EUR, 119.5.EUR))
    expectRejectionWithMessage(order, tooHighPriceMatch, "Invalid price")
  }

  it should "reject matches whose price is off limit when selling" in {
    val tooLowPriceMatch = orderMatch.copy(fiatAmount = Both(80.EUR, 79.5.EUR))
    val sellingOrder = order.copy(
      orderType = Ask,
      amount = 1.BTC,
      price = LimitPrice(Price(109.68.EUR))
    )
    expectRejectionWithMessage(sellingOrder, tooLowPriceMatch, "Invalid price")
  }

  it should "reject matches with inconsistent amounts" in {
    val inconsistentOrderMatch = orderMatch.copy(
      bitcoinAmount = Both(0.5.BTC, 0.5.BTC),
      fiatAmount = orderMatch.fiatAmount.map(_ / 2)
    )
    expectRejectionWithMessage(order, inconsistentOrderMatch, "Match with inconsistent amounts")
  }

  it should "accept valid order matches" in {
    inside(validator.shouldAcceptOrderMatch(order, orderMatch, alreadyBlocking = 0.BTC)) {
      case MatchAccepted(_) =>
    }
  }

  it should "take rounding into account when checking the price limit" in {
    val boundaryOrderMatch = orderMatch.copy(fiatAmount = Both(110.23.EUR,109.68.EUR))
    inside(validator.shouldAcceptOrderMatch(order, boundaryOrderMatch, alreadyBlocking = 0.BTC)) {
      case MatchAccepted(_) =>
    }
  }

  it should "take pending funds requests into account" in {
    expectRejectionWithMessage(order, orderMatch, "Invalid amount", alreadyBlocking = 0.5.BTC)
  }

  it should "accept matches when having a running exchange" in {
    val runningExchange = exchange.copy[Euro.type](
      metadata = exchange.metadata.copy(id = ExchangeId.random())
    ).startHandshaking(
      user = Exchange.PeerInfo("account1", new KeyPair),
      counterpart = Exchange.PeerInfo("account2", new KeyPair)
    )
    val exchangingOrder = order.start.copy(
      amount = 10.BTC,
      exchanges = Map(runningExchange.id -> runningExchange)
    )
    inside(validator.shouldAcceptOrderMatch(exchangingOrder, orderMatch, alreadyBlocking = 0.BTC)) {
      case MatchAccepted(_) =>
    }
  }

  private def expectRejectionWithMessage(order: Order[Euro.type],
                                         orderMatch: OrderMatch[Euro.type],
                                         message: String,
                                         alreadyBlocking: Bitcoin.Amount = 0.BTC): Unit = {
    inside(validator.shouldAcceptOrderMatch(order, orderMatch, alreadyBlocking)) {
      case MatchRejected(completeMessage) =>
        completeMessage should include(message)
    }
  }
}
