package transpiler.codegen.ir.analysis

import transpiler.codegen.ir.{IRExpr, IRNode}

case class SemanticContext(
  variableInfos: Map[IRNode, VariableInfoNode],
  blockInfos: Map[IRNode, BlockInfoNode],
  ifInfos: Map[IRNode, IfInfoNode]
)

