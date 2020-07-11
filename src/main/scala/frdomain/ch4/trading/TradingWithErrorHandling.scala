package frdomain.ch4
package trading

import scalaz._
import Scalaz._

trait TradingWithErrorHandling[Account, Trade, ClientOrder, Order, Execution, Market] {
  type Err[A] = NonEmptyList[String] \/ A
  type Valid[A] = ListT[Err, A]
  type TradingOp[I, O] = Kleisli[Valid, I, O]

  def clientOrders: TradingOp[List[ClientOrder], Order]
  def execute(market: Market, brokerAccount: Account): TradingOp[Order, Execution]
  def allocate(accounts: List[Account]): TradingOp[Execution, Trade]

  def tradeGeneration(market: Market, broker: Account, clientAccounts: List[Account])
      : TradingOp[List[ClientOrder], Trade] = {
    clientOrders               andThen    
    execute(market, broker)    andThen   
    allocate(clientAccounts)
  }
}

