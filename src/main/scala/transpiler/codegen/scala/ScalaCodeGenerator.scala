package transpiler.codegen.scala

import transpiler.codegen.Variable.VariableInfo
import transpiler.codegen.VariableHoister
import transpiler.codegen.ir.*

class ScalaCodeGenerator {
  private val analyzer = new VariableAnalyzer
  private val formatter = new ScalaFormatter
  private val hoister = new VariableHoister
  private val expressGen = new ScalaExpressionGenerator

  def generateFile(ir: IRNode, objectName: String = "PyScalaMain"): String = {
    println("=== Starting Scala Code Generation ===")

    val analysis = analyzer.analyze(ir)

    val hoistedDeclarations = hoister.generateHoistedDeclarations(analysis.variables)
    val hoistedCode = formatter.formatHoistedDeclarations(hoistedDeclarations)

    val mainBody = generateStatement(ir, 2, analysis.variables)

    val fullMainBody = if (hoistedCode.nonEmpty) {
      s"$hoistedCode$mainBody"
    } else {
      mainBody
    }

    formatter.formatObject(objectName, fullMainBody)
  }

  private def generateStatement(ir: IRNode, indentLevel: Int, variables: Map[String, VariableInfo]): String = {
    ir match {
      case IRBlock(statements) =>
        statements.map(generateStatement(_, indentLevel, variables)).mkString("\n")

      case IRAssignment(name, value) =>
        val exprCode = expressGen.generateExpression(value)

        if (hoister.shouldDeclareAsHoisted(name, variables)) {
          formatter.formatAssignment(name, exprCode, indentLevel)
        } else {
          val isReassigned = variables.get(name).exists(_.isReassigned)
          val keyword = if (isReassigned) "var" else "val"
          formatter.formatVariableDeclaration(keyword, name, exprCode, indentLevel)
        }

      case IRReAssignment(name, value) =>
        val exprCode = expressGen.generateExpression(value)
        formatter.formatAssignment(name, exprCode, indentLevel)

      case IRIf(condition, thenBranch, elseBranch) =>
        val condCode = expressGen.generateExpression(condition)
        val thenCode = generateStatement(thenBranch, indentLevel + 1, variables)
        val elseCode = elseBranch.map(generateStatement(_, indentLevel + 1, variables))

        formatter.formatIf(condCode, thenCode, elseCode, indentLevel)

      case IRExprStmt(expr) =>
        val exprCode = expressGen.generateExpression(expr)
        formatter.formatExpressionStatement(exprCode, indentLevel)
    }
  }

  def printAnalysisResults(ir: IRNode): Unit = {
    val analysis = analyzer.analyze(ir)

    println("\n=== Variable Analysis Results ===")
    analysis.variables.toSeq.sortBy(_._1).foreach { case (name, info) =>
      println(s"Variable: $name")
      println(s"  Reassigned: ${info.isReassigned}")
      println(s"  Needs hoisting: ${info.needsHoisting}")
      println(s"  Defined in blocks: ${info.definedInBlocks}")
      println(s"  Used in blocks: ${info.usedInBlocks}")
    }
  }
}
