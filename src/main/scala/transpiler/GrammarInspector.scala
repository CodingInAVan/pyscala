package transpiler

import generated.{Python3Lexer, Python3Parser}
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

object GrammarInspector:
  def listAvailableContextClasses(): Unit =
    println("=== Available Context Classes in Python3Parser ===")

    val clazz = classOf[Python3Parser]
    val innerClasses = clazz.getDeclaredClasses

    val contextClasses = innerClasses
      .filter(_.getSimpleName.endsWith("Context"))
      .map(_.getSimpleName)
      .sorted

    contextClasses.foreach(println)

    println(s"\nTotal context classes: ${contextClasses.length}")

  def listVisitorMethods(): Unit =
    println("\n=== Available Visitor Methods ===")

    val visitorClass = classOf[generated.Python3ParserBaseVisitor[_]]
    val methods = visitorClass.getDeclaredMethods
      .filter(_.getName.startsWith("visit"))
      .map(_.getName)
      .sorted

    methods.foreach(println)

    println(s"\nTotal visitor methods: ${methods.length}")


  def inspectSimpleCode(): Unit =
    val pythonCode = "x = 5"

    val input = CharStreams.fromString(pythonCode)
    val lexer = new Python3Lexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new Python3Parser(tokens)

    val tree = parser.file_input()

    println("\n=== Parser Tree for 'x = 5' ===")
    printTree(tree, parser, 0)

  def printTree(tree: ParseTree, parser: Python3Parser, depth: Int): Unit =
    val indent = "  " * depth
    val className = tree.getClass.getSimpleName
    val text = if (tree.getText.length > 50) tree.getText.take(50) + "..." else tree.getText

    println(s"$indent$className: '$text'")

    for (i <- 0 until tree.getChildCount) {
      printTree(tree.getChild(i), parser, depth + 1)
    }

  def main(args: Array[String]): Unit =
    listAvailableContextClasses()
    listVisitorMethods()
    inspectSimpleCode()


