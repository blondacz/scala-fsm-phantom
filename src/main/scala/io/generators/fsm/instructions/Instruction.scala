package io.generators.fsm.instructions

import io.generators.fsm.instructions.Instruction.InstructionState
import io.generators.fsm.instructions.Instruction.InstructionState._
import io.generators.fsm.instructions.Instruction.InstructionState.Marker._


class Instruction[S <: InstructionState] {

  def publish[T >: S <: New]: Instruction[Published] = {
    println("published")
    new Instruction
  }

  def acknowledge[T >: S <: Acknowledgeable] : Instruction[Instructed] = {
    println("acked")
    new Instruction
  }

  def cancel[T >: S <: Cancellable] : Instruction[CancelSubmitted] = {
    println("cancel submitted")
    new Instruction
  }

  def nack[T >: S <: Acknowledgeable] : Instruction[Failed] = {
    println("nacked")
    new Instruction
  }
}

object Instruction {

  sealed trait InstructionState
  object InstructionState {
    sealed trait New extends InstructionState
    sealed trait Published extends Cancellable with Acknowledgeable
    sealed trait Instructed extends Cancellable
    sealed trait Cancelled extends Terminated
    sealed trait CancelSubmitted extends Acknowledgeable
    sealed trait Failed extends Cancellable
    sealed trait NotInstructed extends Terminated
    //marker states
    object Marker {
      sealed trait Cancellable extends InstructionState
      sealed trait Terminated extends InstructionState
      sealed trait Acknowledgeable extends InstructionState
    }
  }
}

object instructing extends App {

  println("\nFirst instruction flow---")
  val i = new Instruction[New]
  val i2: Instruction[Published] = i.publish
  val i3: Instruction[Instructed] = i2.acknowledge
  val i4: Instruction[CancelSubmitted] = i2.cancel
  val i5 = i2.acknowledge


  println("\nSecond instruction flow---")
  val j = new Instruction[New]
  val j2 : Instruction[Published] = j.publish
  val j3 : Instruction[Failed] = j.nack
  val j4  = j.cancel

}