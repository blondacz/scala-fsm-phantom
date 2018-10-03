package io.generators.fsm.instructions


import ReportableInstances._
import ReportableSyntax._
import cats.Eval
import cats.data.IndexedStateT
import io.generators.fsm.instructions.V3Model.Instruction
import io.generators.fsm.instructions.V3Model.Instruction.ConfirmationState._
import io.generators.fsm.instructions.V3Model.Instruction.MessageState._
import io.generators.fsm.instructions.V3Model.Instruction.ConfirmationState
import io.generators.fsm.instructions.V3Model.Instruction.MessageState
import io.generators.fsm.instructions.V3Model.Instruction.MessageState.Marker._

import scala.reflect.ClassTag

object V3Model {
  type InstructionTransition[OMS <: MessageState,NMS <: MessageState,OCS <: ConfirmationState,NCS <: ConfirmationState] = IndexedStateT[Eval,Instruction[OMS,OCS],Instruction[NMS,NCS],Unit]

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

      def publish : InstructionTransition[New,Published,Unconfirmed,Unconfirmed] = IndexedStateT.set(Instruction("x"))
      def ackNew[C <: ConfirmationState : ClassTag] : InstructionTransition[Published,Instructed,C,C] = IndexedStateT.set(Instruction("x"))
      def confirm[C <: ConfirmationState ] : InstructionTransition[Instructed,Instructed,C,Confirmed] = IndexedStateT.set(Instruction("x"))

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
