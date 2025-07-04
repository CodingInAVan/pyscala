package transpiler

import generated.{Python3Lexer, Python3Parser}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import transpiler.codegen.ir.IRNode

trait TranspilerTestHelpers:
  def transpile(pythonCode: String): IRNode = {
    val input = CharStreams.fromString(pythonCode)
    val lexer = new Python3Lexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new Python3Parser(tokens)
    parser.removeErrorListeners()
    val tree = parser.file_input()
    if (parser.getNumberOfSyntaxErrors > 0) {
      throw new RuntimeException(s"Parse errors in: $pythonCode")
    }
    val visitor = new PythonToIRVisitor()
    visitor.visit(tree)
  }

  def toPythonCode(code: String): String = {
    code.linesIterator.dropWhile(_.trim.isEmpty).toList
      .reverse.dropWhile(_.trim.isEmpty).reverse
      .mkString("\n") + "\n"
  }
