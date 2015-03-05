package coinffeine.peer.payment.okpay

import scala.concurrent.{ExecutionContext, Future}

import coinffeine.model.currency.{CurrencyAmount, FiatAmount, FiatCurrency}
import coinffeine.model.payment.PaymentProcessor.{AccountId, PaymentId}
import coinffeine.model.payment.{AnyPayment, Payment}
import coinffeine.peer.payment.PaymentProcessorException
import coinffeine.peer.payment.okpay.OkPayClient.{FeePolicy, PaidBySender}

trait OkPayClient {

  val accountId: AccountId

  def sendPayment[C <: FiatCurrency](to: AccountId,
                                     amount: CurrencyAmount[C],
                                     comment: String,
                                     feePolicy: FeePolicy = PaidBySender): Future[Payment[C]]

  def findPayment(paymentId: PaymentId): Future[Option[AnyPayment]]

  def currentBalances(): Future[Seq[FiatAmount]]

  def currentBalance[C <: FiatCurrency](currency: C): Future[CurrencyAmount[C]] = {
    implicit val ec = executionContext
    currentBalances().map { balances =>
      balances.find(_.currency == currency)
        .getOrElse(throw new PaymentProcessorException(s"No balance in $currency"))
        .asInstanceOf[CurrencyAmount[C]]
    }
  }

  protected def executionContext: ExecutionContext
}

object OkPayClient {
  sealed trait FeePolicy
  case object PaidByReceiver extends FeePolicy
  case object PaidBySender extends FeePolicy

  abstract class Error(msg: String, cause: Throwable)
    extends Exception(s"invalid OKPay client operation: $msg", cause)

  case class AccountNotFound(account: AccountId, cause: Throwable)
    extends Error(s"account `$account` not found", cause)

  case class AuthenticationFailed(account: AccountId, cause: Throwable)
    extends Error(s"authentication failed for account `$account`", cause)

  case class CurrencyDisabled(cause: Throwable)
    extends Error("currency is disabled", cause)

  case class NotEnoughMoney(account: AccountId, cause: Throwable)
    extends Error(s"not enough funds in account $account", cause)

  case class TransactionNotFound(cause: Throwable)
    extends Error("transaction not found", cause)

  case class UnsupportedPaymentMethod(cause: Throwable)
    extends Error("unsupported payment method", cause)

  case class ReceiverNotFound(receiver: AccountId, cause: Throwable)
    extends Error(s"receiver `$receiver` not found", cause)

  case class InternalError(cause: Throwable)
    extends Error("internal error", cause)

  case class UnexpectedError(cause: Throwable)
    extends Error("unexpected error", cause)
}
