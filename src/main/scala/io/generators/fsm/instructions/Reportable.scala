package io.generators.fsm.instructions

import io.generators.fsm.instructions.Instruction.{ConfirmationState, MessageState}

trait Reportable[T] {
  def report(t: T) : String
  def print(t: T) : Unit = println(report(t))
}

object ReportableSyntax {

  implicit class ReportableSynt[T](subject: T) {
    def report(implicit r: Reportable[T]): String = r.report(subject)
    def print(implicit r: Reportable[T]): Unit = r.print(subject)
  }

}

object ReportableInstances {
  implicit def instructionReportable[S <: MessageState,C <: ConfirmationState] : Reportable[Instruction[S,C]] =
    (i: Instruction[S, C]) => i.toString + " MessageState: " + i.ms.tpe.toString + " ConfirmationState: " + i.cs.tpe.toString
}
