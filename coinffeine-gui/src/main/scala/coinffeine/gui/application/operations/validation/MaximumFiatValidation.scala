package coinffeine.gui.application.operations.validation

import scalaz.NonEmptyList

import coinffeine.model.currency.{CurrencyAmount, FiatCurrency}
import coinffeine.model.market.Order
import coinffeine.peer.amounts.AmountsCalculator

private class MaximumFiatValidation(amountsCalculator: AmountsCalculator) extends OrderValidation {

  override def apply[C <: FiatCurrency](newOrder: Order[C]): OrderValidation.Result = {
    val maximum = amountsCalculator.maxFiatPerExchange(newOrder.price.currency)
    val tooHighRequestOpt = for {
      price <- newOrder.price.toOption
      requestedFiat = price.of(newOrder.amount)
      if requestedFiat > maximum
    } yield requestedFiat
    tooHighRequestOpt.fold[OrderValidation.Result](OrderValidation.OK)(amount =>
      maximumAmountViolated(amount, maximum)
    )
  }

  private def maximumAmountViolated(requested: CurrencyAmount[_], maximum: CurrencyAmount[_]) =
    OrderValidation.Error(NonEmptyList(OrderValidation.Violation(
      title = "Invalid fiat amount",
      description = s"Maximum allowed fiat amount is $maximum, but you requested $requested"
    )))
}
