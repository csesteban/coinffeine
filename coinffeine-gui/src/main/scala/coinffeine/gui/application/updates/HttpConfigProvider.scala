package coinffeine.gui.application.updates

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import com.typesafe.config.{ConfigFactory, Config}
import dispatch.{Http, as, url}

import coinffeine.peer.net.DaemonHttpClient

class HttpConfigProvider extends ConfigVersionChecker.ConfigProvider {

  private val daemonHttp = new DaemonHttpClient
  private val http = new Http(daemonHttp.client)

  override def apply(): Future[Config] = Future {
    // `http.apply()` returns a future, but it's execution takes a long time (~1 second),
    // perhaps due to backend (Netty) machinery initialization. So we execute everything in
    // another future to ensure the function evaluates immediately.
    http(HttpConfigProvider.FileLocation OK as.String).map(ConfigFactory.parseString)
  }.flatMap(identity)

  override def shutdown(): Unit = {
    http.shutdown()
    daemonHttp.shutdown()
  }
}

object HttpConfigProvider {
  val FileLocation = url(
    "https://raw.githubusercontent.com/Coinffeine/coinffeine/master/VERSION")
}
