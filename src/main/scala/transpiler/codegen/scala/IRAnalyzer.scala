package transpiler.codegen.scala

import transpiler.codegen.Variable.{BlockId, VariableInfo}
import transpiler.codegen.ir.IRNode

case class UseIfExprVars(varNames: Set[String])

case class CombinedAnalysisResult(
  variables: Map[String, VariableInfo],
  blockScopes: Map[BlockId, Set[String]],
  canUseIfExprVars: UseIfExprVars
)