package io.generators.fsm.instructions



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
  implicit def instruction1Reportable[S <: V1Model.Instruction.MessageState,C <: V1Model.Instruction.ConfirmationState] : Reportable[V1Model.Instruction[S,C]] =
    (i: V1Model.Instruction[S, C]) => i.toString.padTo(20,' ') + " MessageState: " + i.ms.runtimeClass.getSimpleName.padTo(15,' ') + " ConfirmationState: " + i.cs.runtimeClass.getSimpleName
 implicit def instruction2Reportable[S <: V2Model.Instruction.MessageState,C <: V2Model.Instruction.ConfirmationState] : Reportable[V2Model.Instruction[S,C]] =
    (i: V2Model.Instruction[S, C]) => i.toString.padTo(20,' ') + " MessageState: " + i.ms.runtimeClass.getSimpleName.padTo(15,' ') + " ConfirmationState: " + i.cs.runtimeClass.getSimpleName
implicit def instruction3Reportable[S <: V3Model.Instruction.MessageState,C <: V3Model.Instruction.ConfirmationState] : Reportable[(V3Model.Instruction[S,C],Seq[String])] = new Reportable[(V3Model.Instruction[S, C], Seq[String])] {
  override def report(i: (V3Model.Instruction[S, C], Seq[String])): String =   i._1.toString.padTo(20,' ') + " MessageState: " + i._1.ms.runtimeClass.getSimpleName.padTo(15,' ') + " ConfirmationState: " + i._1.cs.runtimeClass.getSimpleName.padTo(15,' ') + " Events: " + i._2.mkString("; ")
}

}
