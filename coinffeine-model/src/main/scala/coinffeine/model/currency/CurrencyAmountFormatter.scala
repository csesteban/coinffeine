package coinffeine.model.currency

object CurrencyAmountFormatter {

  def format(amount: CurrencyAmount[_], symbolPos: Currency.SymbolPosition): String =
    addSymbol(formatNumericPart(amount), symbolPos, amount.currency)

  private def formatNumericPart(amount: CurrencyAmount[_]): String = s"%s%d%s".format(
    if (amount.units < 0) "-" else "",
    amount.units.abs / amount.currency.unitsInOne,
    formatDecimalPart(amount)
  )

  private def formatDecimalPart(amount: CurrencyAmount[_]): String =
    if (amount.currency.precision == 0) ""
    else {
      val formatString = s".%0${amount.currency.precision}d"
      formatString.format(amount.units.abs % amount.currency.unitsInOne)
    }

  def format(amount: CurrencyAmount[_]): String =
    format(amount, amount.currency.preferredSymbolPosition)

  def formatMissing[C <: Currency](currency: C, symbolPos: Currency.SymbolPosition): String = {
    val amount = if (currency.precision == 0) "_" else "_." + "_" * currency.precision
    addSymbol(amount, symbolPos, currency)
  }

  def formatMissing[C <: Currency](currency: C): String =
    formatMissing(currency, currency.preferredSymbolPosition)

  private def addSymbol[C <: Currency](
      amount: String, symbolPos: Currency.SymbolPosition, currency: C): String =
    symbolPos match {
      case Currency.SymbolPrefixed => s"${currency.symbol}$amount"
      case Currency.SymbolSuffixed => s"$amount${currency.symbol}"
      case Currency.NoSymbol => amount
    }
}
