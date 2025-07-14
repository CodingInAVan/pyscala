package transpiler.codegen.visitor.expr

import generated.{Python3Parser, Python3ParserBaseVisitor}
import transpiler.codegen.ir.{BinaryOp, IRAssignment, IRAugAssignment, IRCall, IRExpr, IRExprStmt, IRGenerator, IRLiteral, IRNode, IRVariable}

import scala.jdk.CollectionConverters.*

class ExpressionVisitor extends Python3ParserBaseVisitor[IRExpr]:


  override def visitAtom_expr(ctx: Python3Parser.Atom_exprContext): IRExpr = {
    println("visitAtom_expr")
    val functionName = ctx.atom().getText
    val argListCtx = ctx.trailer().asScala.headOption.flatMap(t => Option(t.arglist()))

    argListCtx match {
      case Some(argsCtx) =>
        IRCall(IRVariable(functionName), argsCtx.argument().asScala.map(arg => visit(arg.test(0))).toList)
      case None => IRGenerator.convertTextToExpr(functionName)
    }

  }
