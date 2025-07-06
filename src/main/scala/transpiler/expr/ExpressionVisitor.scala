package transpiler.expr

import generated.{Python3Parser, Python3ParserBaseVisitor}
import transpiler.codegen.ir.{BinaryOp, IRAssignment, IRAugAssignment, IRExprStmt, IRGenerator, IRLiteral, IRNode}

class ExpressionVisitor extends Python3ParserBaseVisitor[IRNode]:
  override def visitExpr_stmt(ctx: Python3Parser.Expr_stmtContext): IRNode = {
    println(s"visitExpr: ${ctx.getText}")
    val text = ctx.getText

    if (ctx.augassign() != null) {
      val varName = ctx.testlist_star_expr(0).getText.trim
      val opString = ctx.augassign().getText.trim
      val rhsCtx = ctx.testlist().getText
      val value = IRGenerator.convertTextToExpr(rhsCtx)
      println(s"value = ${value}")

      val op: Option[BinaryOp] = opString match {
        case "+=" => Some(BinaryOp.Add)
        case "-=" => Some(BinaryOp.Sub)
        case "*=" => Some(BinaryOp.Mul)
        case "/=" => Some(BinaryOp.Div)
        case _ => None
      }

      return op.map(binaryOp => IRAugAssignment(varName, binaryOp, value)) match {
        case Some(aug) => aug
        case None =>
          IRExprStmt(IRLiteral("/* Invalid augment assignment. */"))
      }

    } else if (text.contains("=") && !text.contains("==") && !text.contains("!=") && !text.contains("<=") && !text.contains(">=")) {
      val parts = text.split("=", 2)
      if (parts.length == 2) {
        val varName = parts(0).trim
        val valueText = parts(1).trim

        val valueExpr = IRGenerator.convertTextToExpr(valueText)
        println(s"valueExpr = ${valueExpr}")

        return IRAssignment(varName, valueExpr)
      }
    }

    val expr = IRGenerator.convertTextToExpr(text)
    IRExprStmt(expr)
  }
