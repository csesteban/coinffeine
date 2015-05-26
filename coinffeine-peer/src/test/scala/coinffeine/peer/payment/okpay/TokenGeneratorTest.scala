package coinffeine.peer.payment.okpay

import org.joda.time.format.DateTimeFormat
import org.scalatest.{FlatSpec, ShouldMatchers}

class TokenGeneratorTest extends FlatSpec with ShouldMatchers {

  val instance = new TokenGenerator("seedToken")

  "TokenGenerator" must "generate a valid token using a date" in {
    val formatter = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm:ss").withZoneUTC()
    val token = instance.build(formatter.parseDateTime("20/02/2014 14:00:00"))
    token shouldBe "CF49014918670822614A5816F10B8C1047FBD7FF2A653848F60BBE52821C0F72"
  }
}
