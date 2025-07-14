package transpiler.codegen.ir.analysis

import transpiler.codegen.ir.{IRIf, IRNode, IRVariable}

case class AnalyzedIR(
  root: IRNode,
  metadata: Map[IRNode, Set[IRAnnotation]]
)

sealed trait IRAnnotation
case class CanBeLoweredToIFExpr(variable: IRVariable, ifBlock: IRIf) extends IRAnnotation
case class HoistRecommended(variable: IRVariable) extends IRAnnotation
case class TypeInfo(variable: IRVariable, tpe: String) extends IRAnnotation
