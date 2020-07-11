package frdomain.ch5
package free

import scalaz._
import Scalaz._

object AccountRepoState {
  type Error = String
  type AccountMap = Map[String, Account]
  type Err[A] = Error \/ A
  type AccountState[A] = StateT[Err, AccountMap, A]
}

trait AccountRepoInterpreterM[M[_]] {
  def apply[A](action: AccountRepo[A]): M[A]
}

import AccountRepoState._
case class AccountRepoStateInterpreter() 
    extends AccountRepoInterpreterM[AccountState] {
  val step: AccountRepoF ~> AccountState = new (AccountRepoF ~> AccountState) {
    override def apply[A](fa: AccountRepoF[A]): AccountState[A] = fa match {
      case Query(no) => 
        StateT { accountMap => accountMap.get(no) match {
          case Some(account) => \/-(accountMap, account)
          case None => -\/(s"Account not found: $no")
        }}

      case Store(account) => 
        StateT(accountMap => \/-(accountMap + (account.no -> account), ()))
        
      case Delete(no) => 
        StateT(accountMap => \/-((accountMap - no, ())))
    }
  }

  def apply[A](action: AccountRepo[A]): AccountState[A] = 
    action.foldMap(step)
}
