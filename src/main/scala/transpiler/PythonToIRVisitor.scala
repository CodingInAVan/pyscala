package transpiler

import generated.{Python3Parser, Python3ParserBaseVisitor}
import org.antlr.v4.runtime.tree.{ParseTree, RuleNode}
import transpiler.codegen.ir.{IRAssignment, IRBlock, IRExprStmt, IRGenerator, IRIf, IRLiteral, IRNode}

import scala.jdk.CollectionConverters.*

class PythonToIRVisitor extends Python3ParserBaseVisitor[IRNode] {
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

  override def visitTest(ctx: Python3Parser.TestContext): IRNode = {
    IRExprStmt(IRGenerator.convertTextToExpr(ctx.getText))
  }

  override def visitBlock(ctx: Python3Parser.BlockContext): IRNode = {
    val statements = ctx.stmt().asScala.map(visit).toList
    IRBlock(statements)
  }

  override def visitIf_stmt(ctx: Python3Parser.If_stmtContext): IRNode = {
    println(s"visitIf_stmt: ${ctx.getText}")

    val tests = ctx.test().asScala.toList
    val blocks = ctx.block().asScala.toList

    if (tests.isEmpty || blocks.isEmpty) {
      return IRExprStmt(IRLiteral("/* Invalid if statement */"))
    }

    buildIfChain(tests, blocks, 0) match {
      case Some(result) => result
      case None => IRExprStmt(IRLiteral("/* Invalid if chain */"))
    }
  }

  private def buildIfChain(tests: List[Python3Parser.TestContext],
    blocks: List[Python3Parser.BlockContext],
    index: Int): Option[IRNode] = {

    if (index >= tests.length) {
      if (index < blocks.length) {
        Some(visitBlock(blocks(index)))
      } else {
        Some(IRBlock(List.empty))
      }
    } else {
      val condition = visitTest(tests(index))
      val thenBranch = visitBlock(blocks(index))

      val elseBranch = if (index + 1 < tests.length) {
        Some(buildIfChain(tests, blocks, index + 1).asInstanceOf[IRBlock])
      } else if (index + 1 < blocks.length) {
        // final else clause
        Some(visitBlock(blocks(index + 1)))
      } else {
        None
      }

      println(s" Building IRIF for condition ${index}: ${tests(index).getText}")
      (condition, thenBranch, elseBranch) match {
        case (IRExprStmt(expr), thenBlock: IRBlock, Some(elseBlk: IRBlock)) =>
          Some(IRIf(expr, thenBlock, Some(elseBlk)))

        case (IRExprStmt(expr), thenBlock: IRBlock, None) =>
          Some(IRIf(expr, thenBlock, None))

        case _ => None
      }

    }
  }
}
