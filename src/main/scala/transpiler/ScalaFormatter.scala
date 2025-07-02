package transpiler

class ScalaFormatter:

  private val indentSize = 2
  private var currentIndentLevel = 0

  def formatFile(objectName: String, statements: List[String]): String = {
    val formattedStatements = statements.map(formatStatement).mkString("\n")

    s"""object $objectName {
       |  def main(args: Array[String]): Unit = {
       |${addIndent(formattedStatements, 2)}
       |  }
       |}""".stripMargin
  }

  def formatStatement(statement: String): String = {
    val trimmed = statement.trim

    // Handle different statement types
    trimmed match {
      case s if s.startsWith("val ") || s.startsWith("var ") =>
        formatVariableDeclaration(s)
      case s if s.contains(" = ") && !s.startsWith("val") && !s.startsWith("var") =>
        formatAssignment(s)
      case s if s.startsWith("if ") =>
        formatIfStatement(s)
      case s if s.startsWith("for ") =>
        formatForLoop(s)
      case s if s.startsWith("def ") =>
        formatFunctionDefinition(s)
      case _ =>
        trimmed
    }
  }

  /**
   * Format variable declarations with proper spacing
   */
  private def formatVariableDeclaration(statement: String): String = {
    val parts = statement.split("=", 2)
    if (parts.length == 2) {
      val declaration = parts(0).trim
      val value = parts(1).trim
      s"$declaration = $value"
    } else {
      statement
    }
  }

  /**
   * Format assignments with proper spacing
   */
  private def formatAssignment(statement: String): String = {
    val parts = statement.split("=", 2)
    if (parts.length == 2) {
      val variable = parts(0).trim
      val value = parts(1).trim
      s"$variable = $value"
    } else {
      statement
    }
  }

  /**
   * Format if statements (placeholder for future)
   */
  private def formatIfStatement(statement: String): String = {
    // TODO: Handle if/else formatting with proper braces and indentation
    statement
  }

  /**
   * Format for loops (placeholder for future)
   */
  private def formatForLoop(statement: String): String = {
    // TODO: Handle for loop formatting
    statement
  }

  /**
   * Format function definitions (placeholder for future)
   */
  private def formatFunctionDefinition(statement: String): String = {
    // TODO: Handle function definition formatting
    statement
  }

  /**
   * Add consistent indentation to text
   */
  private def addIndent(text: String, indentLevel: Int): String = {
    val indent = " " * (indentLevel * indentSize)
    text.split('\n').map(line =>
      if (line.trim.nonEmpty) s"$indent$line" else line
    ).mkString("\n")
  }

  /**
   * Format expressions with proper operator spacing
   */
  def formatExpression(expression: String): String = {
    expression
      .replaceAll("\\s*\\+\\s*", " + ")
      .replaceAll("\\s*-\\s*", " - ")
      .replaceAll("\\s*\\*\\s*", " * ")
      .replaceAll("\\s*/\\s*", " / ")
      .replaceAll("\\s*%\\s*", " % ")
      .replaceAll("\\s*==\\s*", " == ")
      .replaceAll("\\s*!=\\s*", " != ")
      .replaceAll("\\s*<=\\s*", " <= ")
      .replaceAll("\\s*>=\\s*", " >= ")
      .replaceAll("\\s*<\\s*", " < ")
      .replaceAll("\\s*>\\s*", " > ")
      .replaceAll("\\s*&&\\s*", " && ")
      .replaceAll("\\s*\\|\\|\\s*", " || ")
  }

  /**
   * Remove extra whitespace and clean up formatting
   */
  def cleanupCode(code: String): String = {
    code
      .split('\n')
      .map(_.replaceAll("\\s+", " ").trim) // Replace multiple spaces with single space
      .filter(_.nonEmpty) // Remove empty lines
      .mkString("\n")
  }

  /**
   * Format method calls with proper spacing
   */
  def formatMethodCall(obj: String, method: String, args: List[String]): String = {
    val formattedArgs = args.mkString(", ")
    s"$obj.$method($formattedArgs)"
  }

  /**
   * Format collections (lists, arrays) with proper spacing
   */
  def formatCollection(collectionType: String, elements: List[String]): String = {
    val formattedElements = elements.mkString(", ")
    s"$collectionType($formattedElements)"
  }
