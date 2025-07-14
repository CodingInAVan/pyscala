package transpiler.codegen.ir.analysis

import transpiler.codegen.ir.{IRIf, IRNode}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

case class IfInfoNode(
  override val id: IRNode,
  override val parent: Option[IRNode],
  loweredToIFExpr: List[VariableInfoNode],
  isExpressionConvertible: Boolean
) extends BaseNode(id, parent)


class IRIfAnalyzer(irNodeAnalyzer: IRRootAnalyzer) extends IRAnalyzer[IfInfoNode] {
  private val symbolTable = mutable.Map[IRNode, IfInfoNode]()

  override def analyze(ir: IRNode): Unit = ir match {
    case IRIf(cond, thenBranch, elseBranch) =>


  }

  override def getSymbolTable: Map[IRNode, IfInfoNode] = symbolTable.toMap
}
