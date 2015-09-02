package coinffeine.model.payment.okpay

import scala.math.BigDecimal.RoundingMode

import coinffeine.model.currency._
import coinffeine.model.payment.PaymentProcessor

object OkPayPaymentProcessor extends PaymentProcessor {

  private val FeeRate = BigDecimal(0.005)
  private val MaxFees = Map[FiatCurrency, FiatAmount](
    Euro -> Euro(2.99),
    UsDollar -> UsDollar(2.99),
    PoundSterling -> PoundSterling(2.99),
    HongKongDollar -> HongKongDollar(20),
    SwissFranc -> SwissFranc(2.99),
    AustralianDollar -> AustralianDollar(2.99),
    PolishZloty -> PolishZloty(12),
    Yen -> Yen(300),
    SwedishKrona -> SwedishKrona(25),
    DenmarkKroner -> DenmarkKroner(20),
    CanadianDollar -> CanadianDollar(2.99),
    RussianRouble -> RussianRouble(100),
    Koruny -> Koruny(80),
    CroatianKuna -> CroatianKuna(20),
    HungarianForint -> HungarianForint(1000),
    NorwegianKrone -> NorwegianKrone(20),
    NewZealandDollar -> NewZealandDollar(2.99),
    RomanianNewLeu -> RomanianNewLeu(13),
    TurkishLira -> TurkishLira(9),
    SouthAfricanRand -> SouthAfricanRand(40),
    YuanRenminbi -> YuanRenminbi(2.99)
  )
  private val StepSize = Map[FiatCurrency, FiatAmount](
    Euro -> Euro(2),
    UsDollar -> UsDollar(2),
    PoundSterling -> PoundSterling(2),
    HongKongDollar -> HongKongDollar(17.5),
    SwissFranc -> SwissFranc(2),
    AustralianDollar -> AustralianDollar(3.2),
    PolishZloty -> PolishZloty(8.5),
    Yen -> Yen(300),
    SwedishKrona -> SwedishKrona(20),
    DenmarkKroner -> DenmarkKroner(15),
    CanadianDollar -> CanadianDollar(3),
    RussianRouble -> RussianRouble(150),
    Koruny -> Koruny(55),
    CroatianKuna -> CroatianKuna(15),
    HungarianForint -> HungarianForint(630),
    NorwegianKrone -> NorwegianKrone(20),
    NewZealandDollar -> NewZealandDollar(3.5),
    RomanianNewLeu -> RomanianNewLeu(9),
    TurkishLira -> TurkishLira(6.5),
    SouthAfricanRand -> SouthAfricanRand(30),
    PhilippinePeso -> PhilippinePeso(100),
    SingaporeDollar -> SingaporeDollar(3),
    MalaysianRinggit -> MalaysianRinggit(10),
    TaiwanNewDollar -> TaiwanNewDollar(70),
    IsraeliNewSheqel -> IsraeliNewSheqel(9),
    MexicanPeso -> MexicanPeso(40),
    YuanRenminbi -> YuanRenminbi(15),
    NigerianNairas -> NigerianNairas(450)
  )

  def calculateFee(amount: FiatAmount): FiatAmount = {
    requireSupported(amount.currency)
    val baseFee = roundUp(amount.value * FeeRate, amount.currency)
    capByMaxFee(baseFee.max(minFee(amount.currency)))
  }

  override def bestStepSize(currency: FiatCurrency): FiatAmount = {
    requireSupported(currency)
    StepSize(currency)
  }

  private def minFee(currency: FiatCurrency) = currency.smallestAmount

  private def capByMaxFee(baseFee: FiatAmount) =
    MaxFees.get(baseFee.currency).fold(baseFee) { maxFee =>
      baseFee.min(maxFee)
    }

  private def roundUp(amount: BigDecimal, currency: FiatCurrency): FiatAmount =
    currency.exactAmount(amount.setScale(currency.precision, RoundingMode.UP))

  private def requireSupported(currency: FiatCurrency): Unit = {
    require(FiatCurrency.supported.contains(currency),
      s"$currency is not supported, use one of: ${FiatCurrency.supported.mkString(", ")}")
  }
}
