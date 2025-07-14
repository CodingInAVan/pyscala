package transpiler.codegen.ir.analysis

import transpiler.codegen.ir.*

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

abstract class BaseNode(
  val id: IRNode,
  val parent: Option[IRNode]
)

case class BlockInfoNode (
  override val id: IRNode,
  override val parent: Option[IRNode],
  variables: List[VariableInfoNode],
  isGlobal: Boolean = false
) extends BaseNode(id, parent)

class IRBlockAnalyzer(irRootAnalyzer: IRRootAnalyzer) extends IRAnalyzer[BlockInfoNode] {
  private val symbolTable = mutable.Map[IRNode, BlockInfoNode]()

  override def analyze(ir: IRNode): Unit = {
    
    ir match {
      case IRBlock(statements) => analyze(statements, Some(ir))
      case _ =>
    }
  }


  private def analyze(irNodes: List[IRNode], parentId: Option[IRNode]): Unit = {
    irNodes.foreach {
      case irNode@IRBlock(statements) =>
        registerBlock(irNode, parentId)
        analyze(statements, Some(irNode))
        symbolTable.get(irNode)

      case irIf: IRIf =>
        irRootAnalyzer.analyze(irIf)

      case _ => None
    }
  }


  private def registerBlock(nodeId: IRNode, parentId: Option[IRNode]) = {
    val initialBlock = BlockInfoNode(id = nodeId, parent = None, variables = List.empty)
    symbolTable.put(nodeId, initialBlock)
  }

  private def isAllAssignment(statements: List[IRNode]): Boolean = {
    statements.forall(irNode => irNode match {
      case IRAssignment => true
      case _ => false
    })
  }

  override def getSymbolTable: Map[IRNode, BlockInfoNode] = symbolTable.toMap
}
