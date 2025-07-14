package transpiler.codegen.ir.analysis

import transpiler.codegen.ir.IRNode

import scala.collection.mutable

case class AssignmentInfoNode (
  override val id: IRNode,
  override val parent: Option[IRNode],
  isReassigned: Boolean
) extends BaseNode(id, parent)

class IRAssignmentAnalyzer extends IRAnalyzer[AssignmentInfoNode] {
  private val symbolTable = mutable.Map[IRNode, AssignmentInfoNode]()

  override def analyze(ir: IRNode): Unit = ???
  override def getSymbolTable: Map[IRNode, AssignmentInfoNode] = symbolTable.toMap
}
