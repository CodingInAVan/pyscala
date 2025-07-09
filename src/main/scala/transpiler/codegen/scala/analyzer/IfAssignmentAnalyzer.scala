package transpiler.codegen.scala.analyzer

import transpiler.codegen.Variable.VariableInfo
import transpiler.codegen.ir.{IRAssignment, IRBlock, IRIf, IRNode}

import scala.collection.mutable

class IfAssignmentAnalyzer(variableInfo: mutable.Map[String, VariableInfo]) extends IRAnalyzer {
  val result: mutable.Set[String] = mutable.Set[String]()
  override def analyze(ir: IRNode): AnalyzerResult = {
    traverse(ir)
    IfAssignmentResult(result.toSet)
  }

  private def traverse(node: IRNode): Unit = {
    node match {
      case IRIf(_, thenBranch, Some(elseBranch)) =>
        variableInfo.keys.foreach { varName =>
          if (canUseIfExpression(varName, node))
            result += varName
        }
        traverse(thenBranch)
        traverse(elseBranch)

      case IRIf(_, thenBranch, None) =>
        traverse(thenBranch)

      case IRBlock(statements) =>
        statements.foreach(traverse)

      case _ => ()
    }
  }

  private def canUseIfExpression(variableName: String, ir: IRNode): Boolean = {
    ir match {
      case IRIf(_, thenBranch, Some(elseBranch)) =>
        val info = variableInfo.get(variableName)
        info.exists { varInfo =>
          varInfo.definedInBlocks.size == 2 && // only two definitions
            !varInfo.isReassigned &&
            definesVariableInAllBranches(variableName, thenBranch, elseBranch)
        }
      case _ => false
    }
  }


  private def definesVariableInAllBranches(
    variableName: String,
    thenBranch: IRBlock,
    elseBranch: IRBlock | IRIf): Boolean = {
    val thenDefines = containsAssignment(variableName, thenBranch)
    val elseDefines = elseBranch match {
      case block: IRBlock => containsAssignment(variableName, block)
    }
    thenDefines && elseDefines
  }

  private def containsAssignment(variableName: String, block: IRBlock): Boolean = {
    block.statements.exists {
      case IRAssignment(name, _) if name == variableName => true
      case _ => false
    }
  }
}
