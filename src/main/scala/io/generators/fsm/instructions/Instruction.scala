package io.generators.fsm.instructions

import io.generators.fsm.instructions.Instruction.ConfirmationState.{Confirmed, Unconfirmed}
import io.generators.fsm.instructions.Instruction.{ConfirmationState, MessageState}
import io.generators.fsm.instructions.Instruction.MessageState._
import io.generators.fsm.instructions.Instruction.MessageState.Marker._
import ReportableInstances._
import ReportableSyntax._


case class Instruction[S <: MessageState, C <: ConfirmationState](ref: String) {

  def failGeneration[T >: S <: New]: Instruction[Failed,C] = this.copy()
  def publish[T >: S <: New]: Instruction[Published,C] = this.copy()
  def ackNew[T >: S <: Published] : Instruction[Instructed,C] = this.copy()
  def ackCancel[T >: S <: CancelSubmitted] : Instruction[Cancelled,C] = this.copy()
  def cancel[T >: S <: Cancellable]: Instruction[CancelSubmitted, Unconfirmed] = this.copy()
  def discard[T >: S <: Cancellable]: Instruction[NotInstructed, Unconfirmed] = this.copy()
  def nackNew[T >: S <: Published] : Instruction[Failed,C] = this.copy()
  def nackCancel[T >: S <: CancelSubmitted] : Instruction[NotInstructed,C] = this.copy()
  def confirm[T >: S <: Instructed] : Instruction[Instructed,Confirmed] = this.copy()
}

object Instruction {

  implicit class transitionOps[T](value: T){
    def ~>[B](f: T => B): B = f(value)
  }

  sealed trait ConfirmationState
  object ConfirmationState {
    sealed trait Unconfirmed extends ConfirmationState
    sealed trait Confirmed extends ConfirmationState
  }

  sealed trait MessageState
  object MessageState {
    sealed trait New extends MessageState
    sealed trait Published extends Cancellable
    sealed trait Instructed extends Cancellable
    sealed trait Cancelled extends Final
    sealed trait CancelSubmitted extends MessageState
    sealed trait Failed extends Cancellable
    sealed trait NotInstructed extends Final
    //marker states
    object Marker {
      sealed trait Cancellable extends MessageState
      sealed trait Final extends MessageState
    }
  }
}

object instructing extends App {

  val i = new Instruction[New,Unconfirmed]("aRef")
  val i2: Instruction[Published,Unconfirmed] = i.publish
  val i3: Instruction[Instructed,Unconfirmed] = i2.ackNew
  val i4: Instruction[CancelSubmitted,Unconfirmed] = i3.cancel
  val i5 : Instruction[Cancelled,Unconfirmed] = i4.ackCancel
  i5.print

  val a = new Instruction[New,Unconfirmed]("aRef")
  val a2: Instruction[Published,Unconfirmed] = a.publish
  val a3: Instruction[Instructed,Unconfirmed] = a2.ackNew
  val a4: Instruction[CancelSubmitted,Unconfirmed] = a3.cancel
  val a5 : Instruction[NotInstructed,Unconfirmed] = a4.nackCancel
  a5.print

  val j = new Instruction[New,Unconfirmed]("bRef")
  val j2 : Instruction[Published,Unconfirmed] = j.publish
  val j3 : Instruction[Failed,Unconfirmed] = j2.nackNew
  val j4: Instruction[NotInstructed,Unconfirmed] = j3.discard
  j4.print

  val k = new Instruction[New,Unconfirmed]("cRef")
  val k2 : Instruction[Published,Unconfirmed] = k.publish
  val k3 : Instruction[Instructed,Unconfirmed] = k2.ackNew
  val k4 : Instruction[Instructed,Confirmed] = k3.confirm
  val k5 : Instruction[Instructed,Confirmed] = k4.confirm
  k5.print

  //val k5  = j3.cancel //fails compilation because we can't cancel confirmed instructions



}