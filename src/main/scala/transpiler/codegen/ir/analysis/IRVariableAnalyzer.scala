package transpiler.codegen.ir.analysis

import transpiler.codegen.ir.*

import scala.collection.mutable

case class VariableInfoNode(
  override val id: IRNode,
  override val parent: Option[IRNode],
  name: String,
  isReassigned: Boolean = false,
  definedInBlocks: Set[IRNode] = Set.empty,
  usedInBlocks: Set[IRNode] = Set.empty,
  needsHoisting: Boolean = false,
  inferredType: Option[String] = None
) extends BaseNode(id, parent)

class IRVariableAnalyzer(IRRootAnalyzer: IRRootAnalyzer) extends IRAnalyzer[VariableInfoNode] {
  private val variableInfo = mutable.Map[String, VariableInfoNode]()

  override def analyze(ir: IRNode): Unit = ???

  override def getSymbolTable: Map[IRNode, VariableInfoNode] = ???
}

