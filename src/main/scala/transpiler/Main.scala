package transpiler

import generated.{Python3Lexer, Python3Parser}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

object Main:
  def main(args: Array[String]): Unit = {
    println("=== Inspecting Python3 Grammar ===")
    transpiler.GrammarInspector.main(Array.empty)

    println("\n=== Testing Minimal Visitor ===")

    // Test with simple Python code
    val pythonCode = "x = 5\nx = 10\n"

    println("Input Python Code:")
    println(pythonCode)
    println()

    try {
      val scalaCode = transpile(pythonCode)
      println("Generated Scala Code:")
      println(scalaCode)
    } catch {
      case e: Exception =>
        println(s"Error during transpilation: ${e.getMessage}")
        e.printStackTrace()
    }
  }

  def transpile(pythonCode: String): String = {
    // Create ANTLR input stream from string
    val input = CharStreams.fromString(pythonCode)

    // Create lexer
    val lexer = new generated.Python3Lexer(input)
    val tokens = new CommonTokenStream(lexer)

    // Create parser
    val parser = new generated.Python3Parser(tokens)

    // Add error listener to see what's wrong
    parser.removeErrorListeners()
    parser.addErrorListener(new org.antlr.v4.runtime.BaseErrorListener() {
      override def syntaxError(recognizer: org.antlr.v4.runtime.Recognizer[_, _],
        offendingSymbol: Any, line: Int, charPositionInLine: Int,
        msg: String, e: org.antlr.v4.runtime.RecognitionException): Unit = {
        println(s"Parse error at line $line:$charPositionInLine - $msg")
        println(s"Offending symbol: $offendingSymbol")
      }
    })

    // Parse starting from file_input (top-level rule)
    val tree = parser.file_input()

    // Check for parse errors
    if (parser.getNumberOfSyntaxErrors > 0) {
      throw new RuntimeException(s"${parser.getNumberOfSyntaxErrors} parse errors occurred")
    }

    // Create our minimal visitor and visit the tree
    val visitor = new PythonToScalaVisitor()
    visitor.visit(tree)
  }