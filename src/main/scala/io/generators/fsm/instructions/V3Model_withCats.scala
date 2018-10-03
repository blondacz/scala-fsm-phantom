package io.generators.fsm.instructions


import ReportableInstances._
import ReportableSyntax._
import cats.Eval
import cats.data.{IndexedStateT => State}
import io.generators.fsm.instructions.V3Model.{Instruction, InstructionType, MessageTransition}
import io.generators.fsm.instructions.V3Model.Instruction.ConfirmationState.{Unconfirmed, _}
import io.generators.fsm.instructions.V3Model.Instruction.MessageState.{New, Published, _}
import io.generators.fsm.instructions.V3Model.Instruction.ConfirmationState
import io.generators.fsm.instructions.V3Model.Instruction.MessageState
import io.generators.fsm.instructions.V3Model.Instruction.MessageState.Marker._

import scala.reflect.ClassTag

object V3Model {

  type Event = String

  type InstructionType = Instruction[_ <: V3Model.Instruction.MessageState,_ <: V3Model.Instruction.ConfirmationState]

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


      def swift(ref: String): Instruction[New, Unconfirmed] = new Instruction(ref)

      def publish : MessageTransition[New,Published,Unconfirmed] = State.modify(_.copy())
      def ackNew[C <: ConfirmationState : ClassTag] : MessageTransition[Published,Instructed,C] = State.modify(i => i.copy(ref = i.ref + "-acked"))
      def confirm[C <: ConfirmationState ] : ConfirmationTransition[Instructed,C,Confirmed] = State.modify(_.copy())
      def failGeneration: MessageTransition[New,Failed,Unconfirmed]  = State.modify(_.copy())
      def ackCancel[C <: ConfirmationState : ClassTag]  : MessageTransition[CancelSubmitted, Cancelled,C]  = State.modify(_.copy())
      def cancel : MessageTransition[Instructed, CancelSubmitted, Unconfirmed] =  State.modify(_.copy())
      def discard[C <: ConfirmationState: ClassTag ]:  MessageTransition[Failed, NotInstructed, C]  = State.modify(_.copy())
      def nackNew[C <: ConfirmationState: ClassTag ] : MessageTransition[Published,Failed,C] = State.modify(_.copy())
      def nackCancel[C <: ConfirmationState: ClassTag ] : MessageTransition[CancelSubmitted,NotInstructed,C] = State.modify(_.copy())
    }

  }
}

  object instructingV3Model extends App {

    val process1 = for {
      _ <- publish
      _ <- ackNew
      _ <- cancel
      _ <- ackCancel
    } yield ()

   process1.runS(swift("x")).value.print

    val process2 = for {
      _ <- publish
      _ <- ackNew
      _ <- cancel
      _ <- nackCancel
    } yield ()

   process2.runS(swift("x")).value.print

    val process3 = for {
      _ <- publish
      _ <- nackNew
      _ <- discard
    } yield ()

   process3.runS(swift("x")).value.print

 val process4 = for {
      _ <- publish
      _ <- ackNew
      _ <- confirm
      _ <- confirm
    } yield ()

   process4.runS(swift("x")).value.print

    val process5 = for {
      _ <- failGeneration
      _ <- discard
    } yield ()

   process5.runS(swift("x")).value.print

  }
