package transpiler

import generated.{Python3Parser, Python3ParserBaseVisitor}
import org.antlr.v4.runtime.tree.{ParseTree, RuleNode}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class PythonToScalaVisitor extends Python3ParserBaseVisitor[String] {
  private val symbolTable = new SymbolTable()

  /**
   * Entry Point
   */
  override def visitFile_input(ctx: Python3Parser.File_inputContext): String = {
    val assignments = collectAssignments(ctx)
    symbolTable.analyzeAssignments(assignments)

    val results = for (i <- 0 until ctx.getChildCount) yield {
      val child = ctx.getChild(i)
      println(s"    Child $i: ${child.getClass.getSimpleName} = '${child.getText}'")

      child match
        case context: Python3Parser.StmtContext =>
          visit(context)
        case _ =>
          ""
    }

    val statements = results.filter(_.nonEmpty)
    // Generate scala object wrapper
    s"""object PyScalaMain {
       |  def main(args: Array[String]): Unit = {
       |    ${statements.map(s => s"\t$s").mkString("\n")}
       |  }
       |}""".stripMargin
  }

  private def collectAssignments(ctx: Python3Parser.File_inputContext): List[(String, String)] = {
    val assignments = mutable.ListBuffer[(String, String)]()

    def collectFromNode(node: ParseTree): Unit = {
      node match {
        case exprStmt: Python3Parser.Expr_stmtContext =>
          if (exprStmt.getText.contains("=")) {
            val parts = exprStmt.getText.split("=", 2)
            if (parts.length == 2) {
              val varName = parts(0).trim
              val value = parts(1).trim
              assignments += ((varName, value))
            }
          }
        case _ =>
          for (i <- 0 until node.getChildCount) {
            collectFromNode(node.getChild(i))
          }
      }
    }

    collectFromNode(ctx)
    assignments.toList
  }

  override def visitExpr_stmt(ctx: Python3Parser.Expr_stmtContext): String = {
    val text = ctx.getText

    if (text.contains("=")) {
      val parts = text.split("=", 2)
      if (parts.length == 2) {
        val varName = parts(0).trim
        val value = parts(1).trim

        return symbolTable.handleAssignment(varName, value)
      }
    }

    visitChildren(ctx)
  }

  /**
   * Handle statements
   */
  override def visitStmt(ctx: Python3Parser.StmtContext): String = {
    println(s"visitStmt called with ${ctx.getChildCount} children")

    for (i <- 0 until ctx.getChildCount) {
      val child = ctx.getChild(i)
      println(s"   Stmt child $i: ${child.getClass.getSimpleName} = '${child.getText}")
    }

    val results = for (i <- 0 until ctx.getChildCount) yield {
      visit(ctx.getChild(i))
    }

    results.find(_.nonEmpty).getOrElse("")
  }

  override def visitChildren(node: RuleNode): String = {
    if (node.getChildCount == 0) {
      return handleTerminal(node.getText)
    }

    val className = node.getClass.getSimpleName
    val text = node.getText

    println(s"visitChildren: $className = '$text'")

    if (node.getChildCount == 0) {
      // handle terminal
      return handleTerminal(text)
    }

    val results = for (i <- 0 until node.getChildCount) yield {
      Option(visit(node.getChild(i))).getOrElse("")
    }

    val nonEmptyResults = results.filter(_.nonEmpty)

    className match {
      case name if name.contains("Expr") => nonEmptyResults.mkString(" ")
      case name if name.contains("Stmt") => nonEmptyResults.mkString("; ")
      case _ => nonEmptyResults.mkString(" ")
    }
  }

  private def handleTerminal(text: String): String = {
    text match {
      case "True" => "true"
      case "False" => "false"
      case "None" => "null"
      case "and" => "&&"
      case "or" => "||"
      case "not" => "!"
      case "=" => "="
      case "print" => "println"

      case t if t.matches("""\d+""") => t // numbers
      case t if t.matches("""".*"""") => t // Strings
      case t if t.matches("""'.*'""") => t // Strings

      // Identifiers - keep as is for now
      case t if t.matches("""[a-zA-Z_][a-zA-Z0-9_]*""") => t
      case t if t.trim.isEmpty => ""
      case "\n" | "\r\n" => ""

      case other => other

    }
  }

  override def visit(tree: ParseTree): String = {
    if (tree == null) return ""

    val result = super.visit(tree)
    val safeResult = Option(result).getOrElse("")

    if (safeResult.nonEmpty) {
      println(s"visit(${tree.getClass.getSimpleName}): '${tree.getText}' -> '$safeResult'")
    }
    
    safeResult
  }
}
