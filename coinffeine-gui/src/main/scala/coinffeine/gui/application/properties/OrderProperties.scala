package coinffeine.gui.application.properties

import scalafx.beans.property._
import scalafx.collections.ObservableBuffer

import org.joda.time.DateTime

import coinffeine.model.currency.Bitcoin
import coinffeine.model.order._

trait OrderProperties {
  def orderProperty: ReadOnlyObjectProperty[AnyCurrencyOrder]
  def idProperty: ReadOnlyObjectProperty[OrderId]
  def typeProperty: ReadOnlyObjectProperty[OrderType]
  def createdOnProperty: ReadOnlyObjectProperty[DateTime]
  def amountProperty: ReadOnlyObjectProperty[Bitcoin.Amount]
  def priceProperty: ReadOnlyObjectProperty[AnyOrderPrice]
  def statusProperty: ReadOnlyObjectProperty[OrderStatus]
  def progressProperty: ReadOnlyDoubleProperty
  def isCancellable: ReadOnlyBooleanProperty
  def exchanges: ObservableBuffer[ExchangeProperties]
}
