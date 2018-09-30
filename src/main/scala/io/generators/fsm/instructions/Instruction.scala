package io.generators.fsm.instructions

import io.generators.fsm.instructions.Instruction.InstructionState
import io.generators.fsm.instructions.Instruction.InstructionState._
import io.generators.fsm.instructions.Instruction.InstructionState.Marker._


case class Instruction[S <: InstructionState](ref: String) {

  def failGeneration[T >: S <: New]: Instruction[Failed] = this.copy()
  def publish[T >: S <: New]: Instruction[Published] = this.copy()
  def acknowledge[T >: S <: Acknowledgeable] : Instruction[Instructed] = this.copy()
  def cancel[T >: S <: Cancellable] : Instruction[CancelSubmitted] = this.copy()
  def nack[T >: S <: Acknowledgeable] : Instruction[Failed] = this.copy()
}

object Instruction {

  implicit class transitionOps[T](value: T){
    def ~>[B](f: T => B): B = f(value)
  }

  sealed trait InstructionState
  object InstructionState {
    sealed trait New extends InstructionState
    sealed trait Published extends Cancellable with Acknowledgeable
    sealed trait Instructed extends Cancellable
    sealed trait Cancelled extends Final
    sealed trait CancelSubmitted extends Acknowledgeable
    sealed trait Failed extends Cancellable
    sealed trait NotInstructed extends Final
    //marker states
    object Marker {
      sealed trait Cancellable extends InstructionState
      sealed trait Final extends InstructionState
      sealed trait Acknowledgeable extends InstructionState
    }
  }
}

object instructing extends App {

  val i = new Instruction[New]("aRef")
  val i2: Instruction[Published] = i.publish
  val i3: Instruction[Instructed] = i2.acknowledge
  val i4: Instruction[CancelSubmitted] = i2.cancel
  val i5 = i2.acknowledge


  val j = new Instruction[New]("bRef")
  val j2 : Instruction[Published] = j.publish
  val j3 : Instruction[Failed] = j.nack
  val j4  = j.cancel
}