package transpiler.codegen

object Variable {
  type BlockId = String

  case class VariableInfo(
    name: String,
    isReassigned: Boolean = false,
    definedInBlocks: Set[BlockId] = Set.empty,
    usedInBlocks: Set[BlockId] = Set.empty,
    needsHoisting: Boolean = false,
    inferredType: Option[String] = None
  )
}