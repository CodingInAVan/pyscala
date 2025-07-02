package transpiler

import generated.{Python3Parser, Python3ParserBaseVisitor}
import org.antlr.v4.runtime.tree.{ParseTree, RuleNode}

import scala.collection.mutable

class PythonToScalaVisitor extends Python3ParserBaseVisitor[String] {
  private val symbolTable = new SymbolTable()
  private var assignmentCounts: Map[String, Int] = Map.empty
  private var formatter = new ScalaFormatter()

  /**
   * Entry Point
   */
  override def visitFile_input(ctx: Python3Parser.File_inputContext): String = {
    val assignments = collectAssignments(ctx)

    assignmentCounts = assignments.groupBy(_._1).view.mapValues(_.length).toMap

    val results = for (i <- 0 until ctx.getChildCount) yield {
      val child = ctx.getChild(i)
      println(s"    Child $i: ${child.getClass.getSimpleName} = '${child.getText}'")

      child match
        case context: Python3Parser.StmtContext =>
          visit(context)
        case _ =>
          ""
    }
    val statements = results.filter(_.nonEmpty).map(formatter.formatStatement)

    symbolTable.printSymbolTable()
    formatter.formatFile("PyScalaMain", statements.toList)
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

    if (text.contains("=") && !text.contains("==") && !text.contains("!=") && !text.contains("<=") && !text.contains(">=")) {
      val parts = text.split("=", 2)
      if (parts.length == 2) {
        val varName = parts(0).trim
        val rawValue = parts(1).trim

        val convertedValue = translateValue(rawValue)

        if (!symbolTable.isDeclared(varName)) {
          val willBeMutable = assignmentCounts.getOrElse(varName, 1) > 1

          if (willBeMutable) {
            symbolTable.declareVariable(varName, isMutable = true)
            return s"var $varName = $convertedValue"
          } else {
            symbolTable.declareVariable(varName, isMutable = false)
            return s"val $varName = $convertedValue"
          }
        } else {
          return s"$varName = $convertedValue"
        }
      }
    }

    // Not an assignment, just visit children
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
    println(s"className = ${className}")
    className match {
      case name if name.contains("Expr") => nonEmptyResults.mkString(" ")
      case name if name.contains("Stmt") => nonEmptyResults.mkString("; ")
      case _ => nonEmptyResults.mkString(" ")
    }
  }

  /**
   * Handle atom nodes (which include names/identifiers)
   * Let's use a simple approach and check the text content
   */
  override def visitAtom(ctx: Python3Parser.AtomContext): String = {
    val text = ctx.getText

    // Debug: print what we're working with
    println(s"visitAtom: text='$text', childCount=${ctx.getChildCount}")

    // Simple pattern matching based on text content
    text match {
      case "True" => "true"
      case "False" => "false"
      case "None" => "null"
      case t if t.matches("""\d+""") => t  // Numbers
      case t if t.matches("""\d+\.\d+""") => t  // Floats
      case t if t.startsWith("\"") && t.endsWith("\"") => t  // Double-quoted strings
      case t if t.startsWith("'") && t.endsWith("'") =>
        "\"" + t.substring(1, t.length - 1) + "\""
      case t if t.startsWith("(") && t.endsWith(")") =>
        // Parenthesized expression - visit the content
        visitChildren(ctx)
      case t if t.matches("""[a-zA-Z_][a-zA-Z0-9_]*""") => t  // Identifiers
      case _ =>
        // Fallback to visiting children
        visitChildren(ctx)
    }
  }

  /**
   * Translate values (True -> true, False -> false, etc.)
   */
  private def translateValue(value: String): String = {
    val trimmed = value.trim
    println(s"translateValue input: '$trimmed'")

    val result = trimmed match {
      case "True" => "true"
      case "False" => "false"
      case "None" => "null"
      case v if v.startsWith("'") && v.endsWith("'") && v.length >= 2 =>
        // Convert single quotes to double quotes
        val content = v.substring(1, v.length - 1)
        val converted = "\"" + content + "\""
        println(s"Converting single quotes: '$v' -> '$converted'")
        converted
      case v =>
        println(s"No conversion needed: '$v'")
        v // Numbers, double-quoted strings, expressions, etc.
    }

    println(s"translateValue result: '$result'")
    result
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
      case t if t.matches("""'.*'""") => // Single-quoted strings -> convert to double quotes
        "\"" + t.substring(1, t.length - 1) + "\""

      case t if t.matches("""[a-zA-Z_][a-zA-Z0-9_]*""") => t
      case t if t.trim.isEmpty => ""
      case "\n" | "\r\n" => ""

      case other => other

    }
  }

  override def visit(tree: ParseTree): String = {
    if (tree == null) return ""

    val result = super.visit(tree)
    Option(result).getOrElse("")
  }
}
