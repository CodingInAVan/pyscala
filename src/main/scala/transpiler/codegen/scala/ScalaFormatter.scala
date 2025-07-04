package transpiler.codegen.scala

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

  /**
   * Format hoisted variable declarations
   *
   */
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

  /**
   * Format binary operation
   */
  def formatBinaryOp(left: String, op: String, right: String): String = {
    s"$left $op $right"
  }

  /**
   * Format unary operation
   */
  def formatUnaryOp(op: String, operand: String): String = {
    s"$op$operand"
  }

  /**
   * Format function call
   */
  def formatFunctionCall(func: String, args: List[String]): String = {
    s"$func(${args.mkString(", ")})"
  }
