package frdomain.ch6
package domain
package service

import java.util.Date
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import Kleisli._

import repository.AccountRepository
import scala.concurrent._
import ExecutionContext.Implicits.global

trait AccountServiceTask[Account, Amount, Balance] {
  type AccountOperation[A] = Kleisli[Task, AccountRepository, A]

  def open(no: String, name: String, rate: Option[BigDecimal], openingDate: Option[Date], 
    accountType: AccountType): AccountOperation[Account]

  def close(no: String, closeDate: Option[Date]): AccountOperation[Account]

  def debit(no: String, amount: Amount): AccountOperation[Account]

  def credit(no: String, amount: Amount): AccountOperation[Account]

  def balance(no: String): AccountOperation[Balance]

  def transfer(from: String, to: String, amount: Amount): AccountOperation[(Account, Account)] = for { 
    a <- debit(from, amount)
    b <- credit(to, amount)
  } yield ((a, b))
}

