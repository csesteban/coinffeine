package coinffeine.gui.application.launcher

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import com.typesafe.scalalogging.LazyLogging

import coinffeine.model.network.PeerId
import coinffeine.peer.config.ConfigProvider

class EnsurePeerIdAction(config: ConfigProvider) extends LazyLogging {

  def apply(): Future[Unit] = Future {
    val gatewaySettings = config.messageGatewaySettings()
    if (gatewaySettings.peerId.isEmpty) {
      val peerId = PeerId.random()
      logger.info("Generating a new peer ID: {}", peerId)
      config.saveUserSettings(gatewaySettings.copy(peerId = Some(peerId)))
    }
  }
}
