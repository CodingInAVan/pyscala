package transpiler

import generated.{Python3Lexer, Python3Parser, Python3ParserBaseVisitor}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import transpiler.codegen.ir.*
import transpiler.codegen.scala.ScalaCodeGenerator
import transpiler.codegen.visitor.PythonToIRVisitor

object Main:
  def main(args: Array[String]): Unit = {
    println("=== Inspecting Python3 Grammar ===")
    transpiler.GrammarInspector.main(Array.empty)

    println("\n=== Testing Minimal Visitor ===")

    // Test with simple Python code
    val pythonCode =
      """
        |x = 2
        |if x > 5:
        | b = 3
        |else:
        | b = 2
        |
        |print(b)
        |""".stripMargin

    println("Input Python Code:")
    println(pythonCode)
    println()

    try {
      val scalaCode = transpileWithIR(pythonCode)
      println("Generated Scala Code:")
      println(scalaCode)
    } catch {
      case e: Exception =>
        println(s"Error during transpilation: ${e.getMessage}")
        e.printStackTrace()
    }
  }

  def transpileWithIR(pythonCode: String): String = {
    // Create ANTLR input stream from string
    val input = CharStreams.fromString(pythonCode)
    val lexer = new Python3Lexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new Python3Parser(tokens)

    parser.removeErrorListeners()
    val tree = parser.file_input()

    if (parser.getNumberOfSyntaxErrors > 0) {
      throw new RuntimeException("Parse errors occurred")
    }

    val irVisitor = new PythonToIRVisitor()
    val ir = irVisitor.visit(tree)

    println("Generated IR:")
    println(ir)
    println()

    // Step 3: Generate Scala code from IR with modular architecture
    val codeGenerator = new ScalaCodeGenerator()
    val scalaCode = codeGenerator.generateFile(ir)

    // Print analysis results for debugging
    //codeGenerator.printAnalysisResults(ir)

    scalaCode
  }

  /**
   * Helper to pretty-print IR for debugging
   */
  def printIR(ir: IRNode, indent: Int = 0): Unit = {
    val spaces = "  " * indent

    ir match {
      case IRBlock(statements) =>
        println(s"${spaces}IRBlock:")
        statements.foreach(printIR(_, indent + 1))

      case IRAssignment(name, value) =>
        println(s"${spaces}IRAssignment($name =")
        printIRExpr(value, indent + 1)
        println(s"${spaces})")

      case IRIf(condition, thenBranch, elseBranch) =>
        println(s"${spaces}IRIf:")
        println(s"${spaces}  condition:")
        printIRExpr(condition, indent + 2)
        println(s"${spaces}  then:")
        thenBranch.statements.foreach(printIR(_, indent + 2))
        if (elseBranch.nonEmpty) {
          println(s"${spaces}  else:")
          elseBranch.foreach(printIR(_, indent + 2))
        }

      case IRExprStmt(expr) =>
        println(s"${spaces}IRExprStmt:")
        printIRExpr(expr, indent + 1)
    }
  }

  def printIRExpr(expr: IRExpr, indent: Int = 0): Unit = {
    val spaces = "  " * indent

    expr match {
      case IRLiteral(value, literalType) =>
        println(s"${spaces}IRLiteral($value, $literalType)")

      case IRVariable(name) =>
        println(s"${spaces}IRVariable($name)")

      case IRBinaryOp(left, op, right) =>
        println(s"${spaces}IRBinaryOp($op):")
        printIRExpr(left, indent + 1)
        printIRExpr(right, indent + 1)

      case IRUnaryOp(op, expr) =>
        println(s"${spaces}IRUnaryOp($op):")
        printIRExpr(expr, indent + 1)

      case IRCall(func, args) =>
        println(s"${spaces}IRCall:")
        println(s"${spaces}  func:")
        printIRExpr(func, indent + 2)
        println(s"${spaces}  args:")
        args.foreach(printIRExpr(_, indent + 2))
    }
  }