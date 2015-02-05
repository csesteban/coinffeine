package coinffeine.peer.amounts

import coinffeine.model.currency.{Bitcoin, CurrencyAmount, FiatCurrency}
import coinffeine.model.exchange.Exchange.Amounts

class AmountsCalculatorStub[C <: FiatCurrency](cannedValues: Amounts[C]*) extends AmountsCalculator {

  override def maxFiatPerExchange[C2 <: FiatCurrency](currency: C2): CurrencyAmount[C2] =
    CurrencyAmount(50000, currency)

  override def exchangeAmountsFor[C2 <: FiatCurrency](
      bitcoinAmount: Bitcoin.Amount, fiatAmount: CurrencyAmount[C2]): Amounts[C2] =
    cannedValues.find { value =>
      value.grossBitcoinExchanged == bitcoinAmount && value.grossFiatExchanged == fiatAmount
    }.getOrElse(new UnsupportedOperationException(s"No canned value for ($bitcoinAmount, $fiatAmount)"))
      .asInstanceOf[Amounts[C2]]
}
