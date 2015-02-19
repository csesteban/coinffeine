package coinffeine.gui.application.launcher

import java.io.File
import scala.concurrent.{ExecutionContext, Future}

import com.typesafe.scalalogging.StrictLogging
import org.bitcoinj.core.Wallet

import coinffeine.gui.setup.{SetupConfig, SetupWizard}
import coinffeine.gui.util.FxExecutor
import coinffeine.model.bitcoin.Network
import coinffeine.peer.bitcoin.wallet.SmartWallet
import coinffeine.peer.config.ConfigProvider

class RunWizardAction(configProvider: ConfigProvider, network: => Network) extends StrictLogging {

  def apply(): Future[Unit] =
    mustRunWizard.flatMap(if (_) runSetupWizard() else Future.successful{})(ExecutionContext.global)

  private def mustRunWizard: Future[Boolean] =
    Future(!configProvider.generalSettings().licenseAccepted)(ExecutionContext.global)

  private def runSetupWizard(): Future[Unit] =  {
    implicit val executor = ExecutionContext.global
    for {
      wallet <- loadOrCreateWallet()
      topUpAddress = wallet.delegate.freshReceiveAddress()
      config <- invokeSetupWizard(topUpAddress.toString)
    } yield persistSetupConfiguration(config)
  }

  private def invokeSetupWizard(walletAddress: String): Future[SetupConfig] = {
    Future(new SetupWizard(walletAddress).run())(FxExecutor.asContext)
  }

  private def loadOrCreateWallet(): Future[SmartWallet] = Future {
    val walletFile = configProvider.bitcoinSettings().walletFile
    if (!walletFile.isFile) {
      createWallet(walletFile)
    }
    SmartWallet.loadFromFile(walletFile)
  }(ExecutionContext.global)

  private def createWallet(walletFile: File): Unit = {
    new Wallet(network).saveToFile(walletFile)
    logger.info("Created new wallet at {}", walletFile)
  }

  private def persistSetupConfiguration(setupConfig: SetupConfig): Unit = {
    configProvider.saveUserSettings(
      configProvider.generalSettings().copy(licenseAccepted = true))

    setupConfig.okPayWalletAccess.foreach { access =>
      val okPaySettings = configProvider.okPaySettings()
      configProvider.saveUserSettings(okPaySettings.copy(
        userAccount = Some(access.walletId),
        seedToken = Some(access.seedToken)
      ))
    }
  }
}
