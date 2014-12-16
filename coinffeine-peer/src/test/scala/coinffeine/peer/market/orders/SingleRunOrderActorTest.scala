package coinffeine.peer.market.orders

import coinffeine.model.market._
import coinffeine.peer.exchange.ExchangeActor
import coinffeine.peer.market.submission.SubmissionSupervisor.{KeepSubmitting, InMarket, StopSubmitting}

class SingleRunOrderActorTest extends OrderActorTest {

  "An order actor" should "submit to the broker and receive submission status" in new Fixture {
    givenOfflineOrder()
    submissionProbe.send(actor, InMarket(entry))
    expectProperty { _.status shouldBe InMarketOrder }
  }

  it should "keep submitting to the broker until been cancelled" in new Fixture {
    givenOfflineOrder()
    expectNoMsg(idleTime)
    val reason = "some reason"
    actor ! OrderActor.CancelOrder
    submissionProbe.fishForMessage() {
      case KeepSubmitting(_) => false
      case StopSubmitting(order.id) => true
    }
    expectProperty { _.status shouldBe CancelledOrder }
  }

  it should "reject order matches after being cancelled" in new Fixture {
    givenOfflineOrder()
    actor ! OrderActor.CancelOrder
    shouldRejectAnOrderMatch("Order already finished")
  }

  it should "stop submitting to the broker & report new status once matching is received" in
    new Fixture {
      givenInMarketOrder()
      gatewayProbe.relayMessageFromBroker(orderMatch)
      givenSuccessfulFundsBlocking()
      submissionProbe.fishForMessage() {
        case StopSubmitting(orderId) if orderId == order.id => true
        case _ => false
      }
      expectProperty { _.status shouldBe InProgressOrder }
      expectProperty { _.progress shouldBe 0.0 }
      exchangeActor.probe.send(actor, ExchangeActor.ExchangeSuccess(completedExchange))
      expectProperty { _.status shouldBe CompletedOrder }
    }

  it should "spawn an exchange upon matching" in new Fixture {
    givenInMarketOrder()
    gatewayProbe.relayMessageFromBroker(orderMatch)
    givenSuccessfulFundsBlocking()
    exchangeActor.expectCreation()
  }

  it should "reject new order matches if an exchange is active" in new Fixture {
    givenInMarketOrder()
    gatewayProbe.relayMessageFromBroker(orderMatch)
    givenSuccessfulFundsBlocking()
    exchangeActor.expectCreation()
    shouldRejectAnOrderMatch("Exchange already in progress")
  }

  it should "not reject resubmissions of already accepted order matches" in new Fixture {
    givenInMarketOrder()
    gatewayProbe.relayMessageFromBroker(orderMatch)
    givenSuccessfulFundsBlocking()
    exchangeActor.expectCreation()

    gatewayProbe.relayMessageFromBroker(orderMatch)
    gatewayProbe.expectNoMsg(idleTime)
  }
}
