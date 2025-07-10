package transpiler.codegen.scala

import transpiler.codegen.ir.{IRBlock, IRIf}

class ScalaFormatter:

  private val indentSize = 2
  private var currentIndentLevel = 0

  def formatObject(objectName: String, mainBody: String): String = {
    s"""object $objectName {
       |  def main(args: Array[String]): Unit = {
       |$mainBody
       |  }
       |}""".stripMargin
  }

  def formatHoistedDeclarations(declarations: List[String]): String = {
    if (declarations.isEmpty) {
      ""
    } else {
      declarations.map(decl => s"\t$decl").mkString("\n") + "\n"
    }
  }

  def formatIf(condition: String, thenBody: String, elseBody: Option[String], indentLevel: Int): String = {
    val indent = getIndent(indentLevel)

    elseBody match {
      case Some(elseCode) =>
        s"""${indent}if ($condition) {
           |$thenBody
           |$indent} else {
           |$elseCode
           |$indent}""".stripMargin

      case None =>
        s"""${indent}if ($condition) {
           |$thenBody
           |$indent}""".stripMargin
    }
  }

  def formatBlockOrIf(branch: IRBlock | IRIf, indentLevel: Int): String = branch match {
    case b: IRBlock => formatBlock(b, indentLevel)
    case i: IRIf    => formatNestedIf(i, indentLevel)
  }

  def formatBlock(block: IRBlock, indentLevel: Int): String = {
    block.statements.map(stmt => formatStatement(stmt, indentLevel)).mkString("\n")
  }

  def formatNestedIf(nestedIf: IRIf, indentLevel: Int): String = {
    val cond = "<condition>" // Replace with actual condition formatting
    val thenCode = formatBlockOrIf(nestedIf.thenBranch, indentLevel + 1)
    val elseCode = nestedIf.elseBranch.map(formatBlockOrIf(_, indentLevel + 1))

    formatIf(cond, thenCode, elseCode, indentLevel)
  }

  /**
   * Format variable declaration
   */
  def formatVariableDeclaration(keyword: String, name: String, value: String, indentLevel: Int): String = {
    val indent = getIndent(indentLevel)
    s"$indent$keyword $name = $value"
  }

  /**
   * Format assignment
   */
  def formatAssignment(name: String, value: String, indentLevel: Int): String = {
    val indent = getIndent(indentLevel)
    s"$indent$name = $value"
  }

  /**
   * Format expression statement
   */
  def formatExpressionStatement(expr: String, indentLevel: Int): String = {
    val indent = getIndent(indentLevel)
    s"$indent$expr"
  }

  def getIndent(level: Int): String = {
    " " * (level * indentSize)
  }

  def formatBinaryOp(left: String, op: String, right: String): String = {
    s"$left $op $right"
  }

  def formatUnaryOp(op: String, operand: String): String = {
    s"$op$operand"
  }

  def formatFunctionCall(func: String, args: List[String]): String = {
    s"$func(${args.mkString(", ")})"
  }

  def formatStatement(stmt: transpiler.codegen.ir.IRNode, indentLevel: Int): String = "// TODO: Implement statement formatting"
