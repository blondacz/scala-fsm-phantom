package io.generators.fsm.instructions

import io.generators.fsm.instructions.V2Model.Instruction.ConfirmationState.{Confirmed, Unconfirmed}
import io.generators.fsm.instructions.V2Model.Instruction.{ConfirmationState, MessageState, publish, swift}
import io.generators.fsm.instructions.V2Model.Instruction.MessageState.{New, _}
import io.generators.fsm.instructions.V2Model.Instruction.MessageState.Marker._
import ReportableInstances._
import ReportableSyntax._
import cats.Monoid
import cats.instances.all._
import io.generators.fsm.instructions.V2Model.Instruction._

import scala.reflect.ClassTag

object V2Model {

  type Events = Seq[String]

  implicit val eventsMonoid: Monoid[Events] = new Monoid[Events] {
    override def empty: Events = Nil

    override def combine(x: Events, y: Events): Events = x ++ y
  }

  type StateAndEvents[M <: MessageState, C <: ConfirmationState] = (Instruction[M,C],Events)

  case class Instruction[S <: MessageState, C <: ConfirmationState](ref: String)(implicit val ms: ClassTag[S], val cs: ClassTag[C])

  implicit class transitionOps[A,B](value: (A,B)) {
    def ~>[C](f: A => (C,B))(implicit m: Monoid[B]): (C,B) = {
      val (s,e) = f(value._1)
      (s,m.combine(value._2,e))
    }
  }

  object Instruction {
    //class tags are required if noty concrete state is specified, the cleanest way is to use context bounds

    def failGeneration[C <: ConfirmationState : ClassTag, S <: New ](i: Instruction[S, C]) : StateAndEvents[Failed, C] = (i.copy(),Seq("failed generation"))

    def publish[C <: ConfirmationState : ClassTag, S <: New ](i: Instruction[S, C]): StateAndEvents[Published, C] = (i.copy(),Seq("published"))

    def ackNew[C <: ConfirmationState: ClassTag,  S <: Published](i: Instruction[S, C]): StateAndEvents[Instructed, C] = (i.copy(),Seq("new acked"))

    def ackCancel[C <: ConfirmationState: ClassTag, S <: CancelSubmitted](i: Instruction[S, C]): StateAndEvents[Cancelled, C] = (i.copy(),Seq("cancel acked"))

    def cancel[ S <: Instructed,  C <: Unconfirmed : ClassTag](i: Instruction[S, C]): StateAndEvents[CancelSubmitted, C] = (i.copy(),Seq("cancel submitted"))

    def discard[ C <: ConfirmationState: ClassTag, S <: Cancellable](i: Instruction[S, C]): StateAndEvents[NotInstructed, C] = (i.copy(),Seq("discarded"))

    def nackNew[C <: ConfirmationState: ClassTag,S <: Published](i: Instruction[S, C]): StateAndEvents[Failed, C] = (i.copy(),Seq("new nacked"))

    def nackCancel[C <: ConfirmationState: ClassTag, S <: CancelSubmitted](i: Instruction[S, C]): StateAndEvents[NotInstructed, C] = (i.copy(),Seq("discarded"))

    def confirm[C <: ConfirmationState: ClassTag, S <: Instructed : ClassTag](i: Instruction[S, C]): StateAndEvents[S, Confirmed] = (i.copy(),Seq("confirmed"))

    def swift(ref: String): StateAndEvents[New, Unconfirmed]= (new Instruction(ref),Nil)

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
  import V2Model._
  (swift("aref") ~> publish ~> ackNew ~> cancel ~> ackCancel).print
  (swift("someRef") ~> publish ~> ackNew ~> cancel ~> nackCancel).print
  (swift("bRef") ~> publish ~> nackNew ~> discard).print
  (swift("cRef") ~> publish ~> ackNew ~> confirm ~> confirm).print
  (swift("xRef") ~> failGeneration ~> discard).print
}
