package coinffeine.model.currency

import java.util.{Currency => JavaCurrency}

import org.scalatest.{FlatSpec, ShouldMatchers}

class CurrencyTest extends FlatSpec with ShouldMatchers {

  "US Dollar" must behave like validFiatCurrency(UsDollar, "USD")
  it must behave like validCurrencyWithCents(UsDollar)

  "Euro" must behave like validFiatCurrency(Euro, "EUR")
  it must behave like validCurrencyWithCents(Euro)

  "Bitcoin" must behave like validCurrency(Bitcoin)

  it must "detect invalid amounts" in {
    Bitcoin.exactAmount(0.0000001)
    Bitcoin(1)
    Bitcoin.exactAmount(0)
    Bitcoin.exactAmount(-100)
    an [ArithmeticException] should be thrownBy { Bitcoin.exactAmount(3.1415926535) }
  }

  private def validCurrency(currency: Currency): Unit =  {

    it must "represent amounts in its own currency" in {
      currency(7).currency should be(currency)
    }

    it must "compare amounts of its own currency" in {
      currency(7).tryCompareTo(currency(10)).get should be < 0
      currency(7.5).tryCompareTo(currency(7)).get should be > 0
      currency(2).tryCompareTo(currency(2)).get should be (0)
      object FakeCurrency extends Currency {
        override val precision = 2
        override val symbol = "F"
        override val preferredSymbolPosition = Currency.SymbolSuffixed
      }
      currency(3).tryCompareTo(FakeCurrency(4)) shouldBe 'empty
    }

    it must "add amounts of its own currency" in {
      currency(2) + currency(3) shouldBe currency(5)
    }

    it must "subtract amounts of its own currency" in {
      currency(2) - currency(3) shouldBe currency(-1)
    }

    it must "multiply amounts of its own currency" in {
      currency(2) * 4 shouldBe currency(8)
    }

    it must "divide amounts of its own currency" in {
      currency(10) /% currency(3) shouldBe (3 -> currency(1))
    }

    it must "invert amounts of its own currency" in {
      -currency(2) shouldBe currency(-2)
    }
  }

  private def validFiatCurrency(currency: FiatCurrency, currencyCode: String): Unit = {

    validCurrency(currency)

    it must "return the corresponding Java currency instance" in {
      currency.javaCurrency should be(JavaCurrency.getInstance(currencyCode))
    }
  }

  private def validCurrencyWithCents(currency: FiatCurrency): Unit = {
    it must "detect invalid amounts" in {
      currency(0.01)
      currency(0)
      currency(-3.14)
      a [ArithmeticException] should be thrownBy { currency(0.009) }
    }
  }
}
