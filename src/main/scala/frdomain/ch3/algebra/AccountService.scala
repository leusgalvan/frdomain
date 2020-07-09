package frdomain.ch3
package algebra

import java.util.Date
import util.Try

trait AccountService[Account, Amount, Balance] {
  def open(no: String, name: String, openingDate: Option[Date]): Try[Account]
  def close(account: Account, closeDate: Option[Date]): Try[Account]
  def debit(account: Account, amount: Amount): Try[Account]
  def credit(account: Account, amount: Amount): Try[Account]
  def balance(account: Account): Try[Balance]

  def transfer(from: Account, to: Account, amount: Amount): Try[(Account, Account, Amount)] = for {
    a <- debit(from, amount)
    b <- credit(to, amount)
  } yield (a, b, amount)
}

trait AccountServiceTry[Account, Amount, Balance] {
  def open(no: String, name: String, openingDate: Option[Date]): Option[Account]
  def close(account: Account, closeDate: Option[Date]): Option[Account]
  def debit(account: Account, amount: Amount): Option[Account]
  def credit(account: Account, amount: Amount): Option[Account]
  def balance(account: Account): Option[Balance]

  def transfer(from: Account, 
               to: Account, 
               amount: Amount): Option[(Account, Account, Amount)] = 
    for {
      a <- debit(from, amount)
      b <- credit(to, amount)
    } yield (a, b, amount)
}

trait AccountServiceEither[Account, Amount, Balance] {
  def open(no: String, name: String, openingDate: Option[Date]): Either[Throwable, Account]
  def close(account: Account, closeDate: Option[Date]): Either[Throwable, Account]
  def debit(account: Account, amount: Amount): Either[Throwable, Account]
  def credit(account: Account, amount: Amount): Either[Throwable, Account]
  def balance(account: Account): Either[Throwable, Balance]

  def transfer(from: Account, 
               to: Account, 
               amount: Amount): Either[Throwable, (Account, Account, Amount)] = 
    for {
      a <- debit(from, amount).right
      b <- credit(to, amount).right
    } yield (a, b, amount)
}
