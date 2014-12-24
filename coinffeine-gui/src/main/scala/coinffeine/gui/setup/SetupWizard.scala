package coinffeine.gui.setup

import scalafx.Includes._
import scalafx.stage.WindowEvent

import org.controlsfx.dialog.{Dialog, Dialogs}

import coinffeine.gui.wizard.Wizard

/** Wizard to collect the initial configuration settings */
class SetupWizard(walletAddress: String) extends Wizard[SetupConfig](
    wizardTitle = "Initial setup",
    steps = Seq(
      new LicenseAgreementPane,
      new OkPayWalletDataPane,
      new TopUpStepPane(walletAddress)
    ),
    initialData = SetupConfig(password = None, okPayCredentials = None, okPayWalletAccess = None)) {

  private def onCloseAction(ev: WindowEvent): Unit = {
    val answer = Dialogs.create()
      .title("Quit Coinffeine")
      .message("You will exit Coinffeine. Are you sure?")
      .actions(Dialog.Actions.NO, Dialog.Actions.YES)
      .showConfirm()
    answer match {
      case Dialog.Actions.YES => cancel()
      case _ => ev.consume()
    }
  }

  onCloseRequest = { ev: WindowEvent => onCloseAction(ev) }
}
