package transpiler.expr

import generated.Python3Parser
import org.antlr.v4.runtime.ParserRuleContext
import transpiler.{PythonToScalaVisitor, SymbolTable}
import transpiler.ir.{IRAssignment, IRExpr, IRLiteral, IRNode}

class AssignmentHandler(symbolTable: SymbolTable, visitor: PythonToScalaVisitor) extends PythonExprHandler {
  override def handle(ctx: ParserRuleContext): Either[String, IRNode] = ctx match {
    case exprCtx: Python3Parser.Expr_stmtContext => {
      ???
    }

    case other =>
      Left(s"AssignmentHandler expected Expr_stmtContext but got ${other.getClass.getSimpleName}")

  }

  def handleAssignment(ctx: Python3Parser.Expr_stmtContext): Either[String, IRNode] = {
    /*
    testlist:
    a, b = 1, 2
     */
    val n = ctx.getChildCount
    if (n == 1) {
      return Right(visitor.visit(ctx.getChild(0)).asInstanceOf[IRNode])
    }
    val left = visitor.visit(ctx.getChild(0)).asInstanceOf[IRExpr]
    
    val text = ctx.getText
    if (text.contains("=") && !text.contains("==")) {
      val parts = text.split("=", 2)
      if (parts.length == 2) {
        val varName = parts(0).trim
        val value = parts(1).trim
        IRAssignment(varName, IRLiteral(value))
      }
    }
    ???
  }
}
