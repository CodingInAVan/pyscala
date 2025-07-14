package transpiler.codegen.ir.analysis

import transpiler.codegen.ir.{IRAssignment, IRBlock, IRExpr, IRExprStmt, IRIf, IRNode}

class IRRootAnalyzer {

  private val variableAnalyzer: IRAnalyzer[VariableInfoNode] = new IRVariableAnalyzer(this)
  private val blockAnalyzer: IRAnalyzer[BlockInfoNode] = new IRBlockAnalyzer(this)
  private val ifAnalyzer: IRAnalyzer[IfInfoNode] = new IRIfAnalyzer(this)

  def analyze(ir: IRNode): SemanticContext = {
    analyzeNode(ir)
    SemanticContext(
      variableInfos = variableAnalyzer.getSymbolTable,
      blockInfos = blockAnalyzer.getSymbolTable,
      ifInfos = ifAnalyzer.getSymbolTable
    )
  }

  private def analyzeNode(ir: IRNode): Unit = {
    ir match {
      case irBlock: IRBlock =>
        blockAnalyzer.analyze(irBlock)

      case irAssignment: IRAssignment =>

      case irIf: IRIf =>

      case irExprStmt: IRExprStmt =>
        
      
    }
  }
}
