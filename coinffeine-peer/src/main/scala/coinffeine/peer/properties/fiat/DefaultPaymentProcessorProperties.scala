package coinffeine.peer.properties.fiat

import akka.actor.ActorSystem

import coinffeine.common.akka.event.EventObservedProperty
import coinffeine.model.currency.balance.FiatBalance
import coinffeine.model.util.Cached
import coinffeine.peer.events.fiat.FiatBalanceChanged
import coinffeine.peer.payment.PaymentProcessorProperties

class DefaultPaymentProcessorProperties(implicit system: ActorSystem)
    extends PaymentProcessorProperties {

  override val balances = EventObservedProperty[Cached[FiatBalance]](
    FiatBalanceChanged.Topic, Cached.fresh(FiatBalance.empty)) {
    case FiatBalanceChanged(newBalances) => newBalances
  }
}
