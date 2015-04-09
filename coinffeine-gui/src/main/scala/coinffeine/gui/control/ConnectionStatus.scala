package coinffeine.gui.control

import coinffeine.model.bitcoin.BlockchainStatus
import coinffeine.model.network.PeerId

/** Combines Coinffeine and Bitcoin Connection status information */
case class ConnectionStatus(
    coinffeine: ConnectionStatus.Coinffeine,
    bitcoin: ConnectionStatus.Bitcoin) {

  import ConnectionStatus._

  val description: String = "%s, %s%s".format(
    formatPeerCount(coinffeine.activePeers, "coinffeine"),
    formatPeerCount(bitcoin.activePeers, "bitcoin"),
    formatBlockchainSyncing
  )

  private def formatPeerCount(count: Int, name: String): String = {
    val pluralS = if (count == 1) "" else "s"
    s"$count $name peer$pluralS"
  }

  private def formatBlockchainSyncing: String = {
    bitcoin.blockchainStatus match {
      case BlockchainStatus.NotDownloading => ""
      case download: BlockchainStatus.Downloading =>
        val percent = (download.progress * 100).toInt
        s", syncing blockchain ($percent%)"
    }
  }
}

object ConnectionStatus {

  case class Coinffeine(activePeers: Int = 0, brokerId: Option[PeerId] = None) {
    def connected: Boolean = activePeers > 0 && brokerId.isDefined
  }

  case class Bitcoin(activePeers: Int, blockchainStatus: BlockchainStatus) {
    def connected: Boolean = activePeers > 0
  }
}
