package coinffeine.gui.application.operations.validation

import scalaz.{Failure, NonEmptyList}

import org.scalatest.Inside

import coinffeine.common.properties.MutableProperty
import coinffeine.common.test.UnitTest
import coinffeine.gui.application.operations.validation.OrderValidation._
import coinffeine.model.currency._
import coinffeine.model.currency.balance.FiatBalances
import coinffeine.model.market._
import coinffeine.model.order.{Ask, Bid, LimitPrice, OrderRequest}
import coinffeine.model.util.Cached
import coinffeine.peer.amounts.DefaultAmountsCalculator

class TransferenceLimitValidationTest extends UnitTest with Inside {

  private val newBid = OrderRequest(Bid, 0.5.BTC, LimitPrice(300.EUR))
  private val newAsk = newBid.copy(orderType = Ask)
  private val spread = Spread.empty

  "The available funds requirement" should "not apply for sellers" in new Fixture {
    givenNotEnoughRemainingLimits()
    instance.apply(newAsk, spread) shouldBe Ok
  }

  it should "optionally require fiat limits to be known" in new Fixture {
    givenStaleRemainingLimits()
    inside(instance.apply(newBid, spread)) {
      case Failure(Warning(NonEmptyList(requirement))) =>
        requirement should include ("not possible to check")
    }
  }

  it should "optionally require available remaining limits to cover order needs" in new Fixture {
    givenNotEnoughRemainingLimits()
    inside(instance.apply(newBid, spread)) {
      case Failure(Error(NonEmptyList(requirement))) =>
        requirement should include (
          "This order will exceed your payment processor monthly transference limits")
    }
  }

  it should "not complain when having enough remaining limits" in new Fixture {
    givenEnoughRemainingLimits()
    instance.apply(newBid, spread) shouldBe Ok
  }

  it should "not complain when having no limit for the currency of the order" in new Fixture {
    givenUnboundedLimits()
    instance.apply(newBid, spread) shouldBe Ok
  }


  private trait Fixture {
    private val enoughLimits = balanceWithLimits(450.EUR)
    private val remainingLimits = new MutableProperty(balanceWithoutLimits)

    val instance =
      new TransferenceLimitValidation(new DefaultAmountsCalculator(), remainingLimits)

    protected def givenEnoughRemainingLimits(): Unit = {
      remainingLimits.set(enoughLimits)
    }

    protected def givenUnboundedLimits(): Unit = {
      remainingLimits.set(balanceWithoutLimits)
    }

    protected def givenStaleRemainingLimits(): Unit = {
      remainingLimits.set(enoughLimits.staled)
    }

    protected def givenNotEnoughRemainingLimits(): Unit = {
      remainingLimits.set(balanceWithLimits(1.EUR))
    }

    private def balanceWithoutLimits = balanceWithLimits()

    private def balanceWithLimits(limits: FiatAmount*) = Cached.fresh(FiatBalances(
      amounts = FiatAmounts.fromAmounts(300.EUR),
      blockedAmounts = FiatAmounts.empty,
      remainingLimits = FiatAmounts(limits))
    )
  }
}
