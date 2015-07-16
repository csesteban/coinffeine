package coinffeine.peer.bitcoin.wallet

import java.io.{File, FileInputStream, InputStream}
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext

import org.bitcoinj.core.Wallet.{BalanceType, SendRequest}
import org.bitcoinj.core.{Wallet => _, _}
import org.bitcoinj.wallet.WalletTransaction

import coinffeine.model.Both
import coinffeine.model.bitcoin._
import coinffeine.model.currency._

class SmartWallet(val delegate: Wallet, feeCalculator: BitcoinFeeCalculator) {

  import SmartWallet._

  type Inputs = Set[TransactionOutPoint]

  def this(network: Network, feeCalculator: BitcoinFeeCalculator) = this(new Wallet(network), feeCalculator)

  private case class ListenerExecutor(listener: Listener, context: ExecutionContext) {
    private val task = new Runnable {
      override def run(): Unit = {
        listener.onChange()
      }
    }
    def apply(): Unit = {
      context.execute(task)
    }
  }
  private var listeners: Map[Listener, ListenerExecutor] = Map.empty

  def addListener(listener: Listener)(implicit executor: ExecutionContext) = synchronized {
    listeners += listener -> ListenerExecutor(listener, executor)
  }

  def removeListener(listener: Listener) = synchronized {
    listeners -= listener
  }

  def findTransactionSpendingOutput(outPoint: TransactionOutPoint): Option[ImmutableTransaction] =
    for {
      tx <- Option(delegate.getTransaction(outPoint.getHash))
      output <- Option(tx.getOutput(outPoint.getIndex.toInt))
      input <- Option(output.getSpentBy)
    } yield ImmutableTransaction(input.getParentTransaction)

  def currentReceiveAddress: Address = synchronized {
    delegate.currentReceiveAddress()
  }

  def estimatedBalance: BitcoinAmount = synchronized {
    delegate.getBalance(BalanceType.ESTIMATED)
  }

  def availableBalance: BitcoinAmount = synchronized {
    delegate.getBalance(BalanceType.AVAILABLE)
  }

  def value(tx: MutableTransaction): BitcoinAmount = tx.getValue(delegate)

  def valueSentFromMe(tx: MutableTransaction): BitcoinAmount = tx.getValueSentFromMe(delegate)

  def valueSentToMe(tx: MutableTransaction): BitcoinAmount = tx.getValueSentToMe(delegate)

  def freshKeyPair(): KeyPair = synchronized {
    delegate.freshReceiveKey()
  }

  def createTransaction(inputs: Inputs,
                        amount: BitcoinAmount,
                        to: Address): ImmutableTransaction = synchronized {
    val request = SendRequest.to(to, amount)
    request.fee = feeCalculator.defaultTransactionFee
    request.feePerKb = 0.BTC
    request.coinSelector = new HandpickedCoinSelector(inputs)
    createTransaction(request)
  }

  def createTransaction(amount: BitcoinAmount, to: Address): ImmutableTransaction = synchronized {
    createTransaction(SendRequest.to(to, amount))
  }

  private def createTransaction(request: SendRequest): ImmutableTransaction = {
    val result = try {
      delegate.sendCoinsOffline(request)
    } catch {
      case ex: InsufficientMoneyException =>
        throw new NotEnoughFunds("Cannot create transaction", ex)
    }
    ImmutableTransaction(result.tx)
  }

  def createMultisignTransaction(requiredSignatures: Both[PublicKey],
                                 amount: BitcoinAmount,
                                 fee: BitcoinAmount = Bitcoin.zero): ImmutableTransaction =
    createMultisignTransaction(collectFunds(amount), requiredSignatures, amount, fee)

  def createMultisignTransaction(inputs: Inputs,
                                 requiredSignatures: Both[PublicKey],
                                 amount: BitcoinAmount,
                                 fee: BitcoinAmount): ImmutableTransaction = synchronized {
    require(amount.isPositive, s"Amount to block must be greater than zero ($amount given)")
    require(!fee.isNegative, s"Fee should be non-negative ($fee given)")
    val totalInputFunds = valueOf(inputs)
    if (totalInputFunds < amount + fee) {
      throw new NotEnoughFunds(
        s"""Not enough funds: $totalInputFunds is not enough for
           |putting $amount in multisig with a fee of $fee""".stripMargin)
    }

    val tx = new MutableTransaction(delegate.getNetworkParameters)
    inputs.foreach { outPoint =>
      tx.addInput(outPoint.getConnectedOutput)
    }
    tx.addMultisigOutput(amount, requiredSignatures.toSeq)
    tx.addChangeOutput(totalInputFunds, amount + fee, delegate.getChangeAddress)

    delegate.signTransaction(SendRequest.forTx(tx))
    ImmutableTransaction(tx)
  }

  def spendCandidates: Seq[MutableTransactionOutput] = synchronized {
    delegate.calculateAllSpendCandidates(ExcludeImmatureCoinBases)
      .filter(!_.getParentTransaction.isPending)
  }

  private def update(): Unit = synchronized {
    listeners.values.foreach(_.apply())
  }

  private def collectFunds(amount: BitcoinAmount): Inputs = {
    val inputFundCandidates = spendCandidates
    val necessaryInputCount =
      inputFundCandidates.view.scanLeft(Bitcoin.zero)(_ + _.getValue)
        .takeWhile(_ < amount)
        .length
    inputFundCandidates.take(necessaryInputCount).map(_.getOutPointFor).toSet
  }

  private def commitTransaction(tx: MutableTransaction): Unit = {
    def containsTransaction(pool: WalletTransaction.Pool) =
      delegate.getTransactionPool(pool).contains(tx.getHash)
    if (!WalletTransaction.Pool.values().exists(containsTransaction)) {
      delegate.commitTx(tx)
    }
  }

  private def contains(tx: MutableTransaction): Boolean = getTransaction(tx.getHash).isDefined

  private def getTransaction(txHash: Hash) = Option(delegate.getTransaction(txHash))

  private def valueOf(inputs: Inputs): BitcoinAmount = inputs.toSeq.map(valueOf).sum

  private def valueOf(input: TransactionOutPoint): BitcoinAmount =
    Option(input.getConnectedOutput)
      .getOrElse(delegate.getTransaction(input.getHash).getOutput(input.getIndex.toInt))
      .getValue

  private def moveToPool(tx: MutableTransaction, pool: WalletTransaction.Pool): Unit = {
    val wtxs = delegate.getWalletTransactions
    delegate.clearTransactions(0)
    delegate.addWalletTransaction(new WalletTransaction(pool, tx))
    wtxs.foreach { wtx =>
      if (tx.getHash != wtx.getTransaction.getHash) {
        delegate.addWalletTransaction(wtx)
      }
    }
  }

  delegate.addEventListener(new AbstractWalletEventListener {

    override def onTransactionConfidenceChanged(wallet: Wallet, tx: MutableTransaction): Unit = {
      // Don't notify confidence changes for already confirmed transactions to reduce load
      if (tx.getConfidence.getConfidenceType != TransactionConfidence.ConfidenceType.BUILDING ||
        tx.getConfidence.getDepthInBlocks == 1) {
        onChange()
      }
    }

    override def onChange(): Unit = {
      update()
    }
  })
}

object SmartWallet {

  private val ExcludeImmatureCoinBases = true

  trait Listener {
    def onChange(): Unit
  }

  def loadFromFile(file: File, feeCalculator: BitcoinFeeCalculator): SmartWallet = {
    val stream = new FileInputStream(file)
    try {
      loadFromStream(stream, feeCalculator)
    } finally {
      stream.close()
    }
  }

  def loadFromStream(stream: InputStream, feeCalculator: BitcoinFeeCalculator): SmartWallet = {
    new SmartWallet(Wallet.loadFromFileStream(stream), feeCalculator)
  }

  case class NotEnoughFunds(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause)
}
