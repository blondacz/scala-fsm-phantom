package io.generators.fsm.instructions


import ReportableInstances._
import ReportableSyntax._
import cats.Eval
import cats.data.{IndexedStateT => State}
import io.generators.fsm.instructions.V3Model.Instruction
import io.generators.fsm.instructions.V3Model.Instruction.ConfirmationState._
import io.generators.fsm.instructions.V3Model.Instruction.MessageState._
import io.generators.fsm.instructions.V3Model.Instruction.ConfirmationState
import io.generators.fsm.instructions.V3Model.Instruction.MessageState
import io.generators.fsm.instructions.V3Model.Instruction.MessageState.Marker._

import scala.reflect.ClassTag

object V3Model {

  type MessageTransition[OMS <: MessageState,NMS <: MessageState,C <: ConfirmationState] = State[Eval,Instruction[OMS,C],Instruction[NMS,C],Unit]
  type ConfirmationTransition[MS <: MessageState,OCS <: ConfirmationState,NCS <: ConfirmationState] = State[Eval,Instruction[MS,OCS],Instruction[MS,NCS],Unit]

  case class Instruction[S <: MessageState, C <: ConfirmationState](ref: String)(implicit val ms: ClassTag[S], val cs: ClassTag[C])

  object Instruction {
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

      def publish : MessageTransition[New,Published,Unconfirmed] = State.modify(_.copy())
      def ackNew[C <: ConfirmationState : ClassTag] : MessageTransition[Published,Instructed,C] = State.modify(i => i.copy(ref = i.ref + "-acked"))
      def confirm[C <: ConfirmationState ] : ConfirmationTransition[Instructed,C,Confirmed] = State.modify(_.copy())

    }

  }
}

  object instructingV3Model extends App {

    val process = for {
      _ <- publish
      _ <- ackNew
      _ <- confirm
      _ <- confirm
    } yield ()

   val run  = process.runS(Instruction[New,Unconfirmed]("x")).value
    run.print

  }
