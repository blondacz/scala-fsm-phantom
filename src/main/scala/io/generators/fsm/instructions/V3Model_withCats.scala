package io.generators.fsm.instructions


import ReportableInstances._
import ReportableSyntax._
import cats.Eval
import cats.Eval.now
import cats.data.{IndexedStateT => State}
import io.generators.fsm.instructions.V3Model.{Instruction, InstructionType, MessageTransition}
import io.generators.fsm.instructions.V3Model.Instruction.ConfirmationState.{Unconfirmed, _}
import io.generators.fsm.instructions.V3Model.Instruction.MessageState.{New, Published, _}
import io.generators.fsm.instructions.V3Model.Instruction.ConfirmationState
import io.generators.fsm.instructions.V3Model.Instruction.MessageState
import io.generators.fsm.instructions.V3Model.Instruction.MessageState.Marker._

import scala.reflect.ClassTag

object V3Model {

  type Event = Seq[String]

  type InstructionType = Instruction[_ <: V3Model.Instruction.MessageState,_ <: V3Model.Instruction.ConfirmationState]

  type MessageTransition[OMS <: MessageState,NMS <: MessageState,C <: ConfirmationState] = State[Eval,Instruction[OMS,C],Instruction[NMS,C],Event]
  type ConfirmationTransition[MS <: MessageState,OCS <: ConfirmationState,NCS <: ConfirmationState] = State[Eval,Instruction[MS,OCS],Instruction[MS,NCS],Event]

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

      def publish : MessageTransition[New,Published,Unconfirmed] = State(i => now((i.copy(),Seq("published"))))
      def ackNew[C <: ConfirmationState : ClassTag] : MessageTransition[Published,Instructed,C] = State(i => now((i.copy(),Seq("acked"))))
      def confirm[C <: ConfirmationState ] : ConfirmationTransition[Instructed,C,Confirmed] = State(i => now((i.copy(),Seq("confirmed"))))
      def failGeneration: MessageTransition[New,Failed,Unconfirmed]  = State(i => now((i.copy(),Seq("failed generation"))))
      def ackCancel[C <: ConfirmationState : ClassTag]  : MessageTransition[CancelSubmitted, Cancelled,C]  = State(i => now((i.copy(),Seq("cancel acked"))))
      def cancel : MessageTransition[Instructed, CancelSubmitted, Unconfirmed] =  State(i => now((i.copy(),Seq("cancel submitted"))))
      def discard[C <: ConfirmationState: ClassTag ]:  MessageTransition[Failed, NotInstructed, C]  = State(i => now((i.copy(),Seq("discarded"))))
      def nackNew[C <: ConfirmationState: ClassTag ] : MessageTransition[Published,Failed,C] = State(i => now((i.copy(),Seq("new nacked"))))
      def nackCancel[C <: ConfirmationState: ClassTag ] : MessageTransition[CancelSubmitted,NotInstructed,C] = State(i => now((i.copy(),Seq("cancel nacked"))))
    }

  }
}

  object instructingV3Model extends App {

    val process1 = for {
      a <- publish
      b <- ackNew
      c <- cancel
      d <- ackCancel
    } yield a ++ b ++ c ++ d

   process1.run(swift("x")).value.print

    val process2 = for {
      a <- publish
      b <- ackNew
      c <- cancel
      d <- nackCancel
    } yield a ++ b ++ c ++ d

   process2.run(swift("x")).value.print

    val process3 = for {
      a <- publish
      b <- nackNew
      c <- discard
    } yield a ++ b ++ c

   process3.run(swift("x")).value.print

 val process4 = for {
      a <- publish
      b <- ackNew
      c <- confirm
      d <- confirm
    } yield a ++ b ++ c ++ d

   process4.run(swift("x")).value.print

    val process5 = for {
      a <- failGeneration
      b <- discard
    } yield a ++ b

   process5.run(swift("x")).value.print

  }
