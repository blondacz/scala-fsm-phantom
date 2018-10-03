package io.generators.fsm.instructions

import io.generators.fsm.instructions.V1Model.Instruction.ConfirmationState.{Confirmed, Unconfirmed}
import io.generators.fsm.instructions.V1Model.Instruction.{ConfirmationState, MessageState, swift}
import io.generators.fsm.instructions.V1Model.Instruction.MessageState._
import io.generators.fsm.instructions.V1Model.Instruction.MessageState.Marker._
import ReportableInstances._
import ReportableSyntax._

import scala.reflect.ClassTag

object V1Model {

  case class Instruction[S <: MessageState, C <: ConfirmationState](ref: String)(implicit val ms: ClassTag[S], val cs: ClassTag[C]) {

    def failGeneration[T >: S <: New]: Instruction[Failed, C] = this.copy()

    def publish[T >: S <: New]: Instruction[Published, C] = this.copy()

    def ackNew[T >: S <: Published]: Instruction[Instructed, C] = this.copy()

    def ackCancel[T >: S <: CancelSubmitted]: Instruction[Cancelled, C] = this.copy()

    def cancel[T >: S <: Cancellable, R >: C <: Unconfirmed]: Instruction[CancelSubmitted, C] = this.copy()

    def discard[T >: S <: Cancellable]: Instruction[NotInstructed, Unconfirmed] = this.copy()

    def nackNew[T >: S <: Published]: Instruction[Failed, C] = this.copy()

    def nackCancel[T >: S <: CancelSubmitted]: Instruction[NotInstructed, C] = this.copy()

    def confirm[T >: S <: Instructed]: Instruction[Instructed, Confirmed] = this.copy()
  }

  object Instruction {

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
object instructingV1Model extends App {

  swift("aRef").publish.ackNew.cancel.ackCancel.print

  swift("someRef").publish.ackNew.cancel.nackCancel.print

  swift("bRef").publish.nackNew.discard.print

  swift("cRef").publish.ackNew.confirm.confirm.print

  swift("xRef").failGeneration.discard.print

  //swift("xRef").publish.ackNew.confirm.cancel //fails compilation because we can't cancel confirmed instructions
}
