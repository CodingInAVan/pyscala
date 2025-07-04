package transpiler

import generated.{Python3Parser, Python3ParserBaseVisitor}
import org.antlr.v4.runtime.tree.{ParseTree, RuleNode}
import transpiler.ir.*

import scala.collection.mutable.ArrayBuffer

class PythonToScalaVisitor extends Python3ParserBaseVisitor[IRNode] {
  /**
   * Entry Point
   */
  override def visitFile_input(ctx: Python3Parser.File_inputContext): IRNode = {
    println(s"visitFile_input: childCount=${ctx.getChildCount}")

    val statements = for (i <- 0 until ctx.getChildCount) yield {
      val child = ctx.getChild(i)
      println(s"  Child $i: ${child.getClass.getSimpleName} = '${child.getText}'")

      child match {
        case stmtCtx: Python3Parser.StmtContext =>
          println(s"    -> Processing as StmtContext")
          Some(visit(stmtCtx))
        case _ =>
          println(s"    -> Skipping (not a StmtContext)")
          None
      }
    }
    println(statements)
    val validStatements = statements.flatten.toList
    println(s"Valid statements found: ${validStatements}")

    IRBlock(validStatements)
  }

  override def visitStmt(ctx: Python3Parser.StmtContext): IRNode = {
    println(s"visitStmt: ${ctx.getText}")

    if (ctx.simple_stmts() != null) {
      println("  -> Visiting simple_stmts")
      val result = visit(ctx.simple_stmts())
      result
    } else if (ctx.compound_stmt() != null) {
      println("  -> Visiting compound_stmt")
      val result = visit(ctx.compound_stmt())
      result
    } else {
      println("  -> No known statement type - creating fallback")
      IRExprStmt(IRLiteral("/* Unknown statement */"))
    }
  }

  override def visitExpr_stmt(ctx: Python3Parser.Expr_stmtContext): IRNode = {
    val text = ctx.getText
    if (text.contains("=") && !text.contains("==") && !text.contains("!=") && !text.contains("<=") && !text.contains(">=")) {
      val parts = text.split("=", 2)
      if (parts.length == 2) {
        val varName = parts(0).trim
        val valueText = parts(1).trim

        val valueExpr = IRGenerator.convertTextToExpr(valueText)

        return IRAssignment(varName, valueExpr)
      }
    }

    val expr = IRGenerator.convertTextToExpr(text)
    IRExprStmt(expr)
  }

  override def visitSimple_stmts(ctx: Python3Parser.Simple_stmtsContext): IRNode = {
    val statements = for (i <- 0 until ctx.getChildCount) yield {
      val child = ctx.getChild(i)
      child match
        case context: Python3Parser.Simple_stmtContext =>
          Some(visit(context))
        case _ =>
          None
    }

    val validStatements = statements.flatten.filter(_ != null).toList

    if (validStatements.length == 1) {
      validStatements.head
    } else if (validStatements.length > 1) {
      IRBlock(validStatements)
    } else {
      IRExprStmt(IRLiteral("/* Empty simple statements */"))
    }
  }

  override def visitSimple_stmt(ctx: Python3Parser.Simple_stmtContext): IRNode = {
    val result = visitChildren(ctx)

    if (result == null) {
      IRExprStmt(IRLiteral("/* Null simple statement */"))
    } else {
      result
    }
  }

  override def visitIf_stmt(ctx: Python3Parser.If_stmtContext): IRNode = {
    val nTests = ctx.test().size()
    val nBlocks = ctx.block().size()
    // initial if condition
    val testCtx = ctx.test(0)
    println(s"  test(0) type: ${testCtx.getClass.getSimpleName}")
    println(s"  test(0) text: ${testCtx.getText}")
    val testExpr = visit(ctx.test(0))
    println(s"testExpr = ${testExpr}")
    if(nBlocks == nTests + 1) {

    } else if(nBlocks == nTests) {

    } else {
      IRExprStmt(IRLiteral("/* nBlocks does not match with nTests */"))
    }
    println(s"nTest = ${nTests} and nBlocks = ${nBlocks}")
    val irBlocks = new ArrayBuffer[IRNode]()
    for (i <- 0 until nBlocks) {
      val block = ctx.block(i)
      println(s"block = ${block.getText}")
      val nStmt = block.stmt().size()

      val irNodes = new ArrayBuffer[IRNode]()
      for (j <- 0 until nStmt) {
        val irNode = visit(block.stmt(i))
        irNodes.append(irNode)
      }
      irBlocks.append(IRBlock(irNodes.toList))
    }
    IRBlock(irBlocks.toList)
    //IRIf(cond=, thenBranch=irBlocks.toList)
  }

  override def visitTest(ctx: Python3Parser.TestContext): IRNode = {
    IRExprStmt(IRGenerator.convertTextToExpr(ctx.getText))
  }
}
