package transpiler.codegen

import transpiler.codegen.ir.IRExpr

object Variable {
  type BlockId = String

  case class VariableInfo(
    name: String,
    isReassigned: Boolean = false,
    definedInBlocks: Set[BlockId] = Set.empty,
    usedInBlocks: Set[BlockId] = Set.empty,
    needsHoisting: Boolean = false,
    inferredType: Option[String] = None,
    assignedValues: List[IRExpr] = List.empty
  )
}