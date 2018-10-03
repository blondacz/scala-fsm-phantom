package io.generators.fsm.instructions

import io.generators.fsm.instructions.V2Model.Instruction.ConfirmationState.{Confirmed, Unconfirmed}
import io.generators.fsm.instructions.V2Model.Instruction.{ConfirmationState, MessageState, publish, swift}
import io.generators.fsm.instructions.V2Model.Instruction.MessageState.{New, _}
import io.generators.fsm.instructions.V2Model.Instruction.MessageState.Marker._
import ReportableInstances._
import ReportableSyntax._
import io.generators.fsm.instructions.V2Model.Instruction._

import scala.reflect.ClassTag

object V2Model {

  case class Instruction[S <: MessageState, C <: ConfirmationState](ref: String)(implicit val ms: ClassTag[S], val cs: ClassTag[C])

  implicit class transitionOps[T](value: T) {
    def ~>[B](f: T => B): B = f(value)
  }

  object Instruction {
    //class tags are required if noty concrete state is specified, the cleanest way is to use context bounds

    def failGeneration[C <: ConfirmationState : ClassTag, S <: New ](i: Instruction[S, C]) : Instruction[Failed, C] = i.copy()

    def publish[C <: ConfirmationState : ClassTag, S <: New ](i: Instruction[S, C]): Instruction[Published, C] = i.copy()

    def ackNew[C <: ConfirmationState: ClassTag,  S <: Published](i: Instruction[S, C]): Instruction[Instructed, C] = i.copy()

    def ackCancel[C <: ConfirmationState: ClassTag, S <: CancelSubmitted](i: Instruction[S, C]): Instruction[Cancelled, C] = i.copy()

    def cancel[ S <: Instructed,  C <: Unconfirmed](i: Instruction[S, C]): Instruction[CancelSubmitted, Unconfirmed] = i.copy()

    def discard[ C <: ConfirmationState: ClassTag, S <: Cancellable](i: Instruction[S, C]): Instruction[NotInstructed, Unconfirmed] = i.copy()

    def nackNew[C <: ConfirmationState: ClassTag,S <: Published](i: Instruction[S, C]): Instruction[Failed, C] = i.copy()

    def nackCancel[C <: ConfirmationState: ClassTag, S <: CancelSubmitted](i: Instruction[S, C]): Instruction[NotInstructed, C] = i.copy()

    def confirm[C <: ConfirmationState: ClassTag, S <: Instructed](i: Instruction[S, C]): Instruction[Instructed, Confirmed] = i.copy()

    def swift(ref: String): Instruction[New, Unconfirmed] = new Instruction(ref)

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
}

object instructingV2Model extends App {
  (swift("aref") ~> publish ~> ackNew ~> cancel ~> ackCancel).print
  (swift("someRef") ~> publish ~> ackNew ~> cancel ~> nackCancel).print
  (swift("bRef") ~> publish ~> nackNew ~> discard).print
  (swift("cRef") ~> publish ~> ackNew ~> confirm ~> confirm).print
  (swift("xRef") ~> failGeneration ~> discard).print
}
