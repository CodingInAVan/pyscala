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

    val mainBody = generateStatement(ir, 2, analysis)

    val fullMainBody = if (hoistedCode.nonEmpty) {
      s"$hoistedCode$mainBody"
    } else {
      mainBody
    }

    formatter.formatObject(objectName, fullMainBody)
  }

  private def generateStatement(ir: IRNode, indentLevel: Int, analysis: CombinedAnalysisResult): String = {
    ir match {
      case IRBlock(statements) =>
        statements.map(generateStatement(_, indentLevel, analysis)).mkString("\n")

      case IRAssignment(name, value) =>
        val exprCode = expressGen.generateExpression(value)

        if (hoister.shouldDeclareAsHoisted(name, analysis.variables)) {
          formatter.formatAssignment(name, exprCode, indentLevel)
        } else {
          val isReassigned = analysis.variables.get(name).exists(_.isReassigned)
          println(s"Variable ${name} is reassigned? ${isReassigned}")
          val keyword = if (isReassigned) "var" else "val"
          formatter.formatVariableDeclaration(keyword, name, exprCode, indentLevel)
        }

      case IRReAssignment(name, value) =>
        val exprCode = expressGen.generateExpression(value)
        formatter.formatAssignment(name, exprCode, indentLevel)

      case IRIf(condition, thenBranch, elseBranchOpt) =>
        elseBranchOpt match
          case Some(elseBranch) =>
            val branchIds = analyzer.extractAllBlockIds(thenBranch) ++ analyzer.extractAllBlockIds(elseBranch)
            val commonVars = analysis.canUseIfExprVars.varNames.filter { varName =>
              analysis.variables(varName).definedInBlocks.subsetOf(branchIds)
            }

            if (commonVars.nonEmpty) {
              commonVars.map { varName =>
                val thenValue = extractAssignmentValue(varName, thenBranch)
                val elseValue = extractAssignmentValue(varName, elseBranch)

                val keyword = "val"
                val conditionExpr = expressGen.generateExpression(condition)
                val thenExpr = expressGen.generateExpression(thenValue)
                val elseExpr = expressGen.generateExpression(elseValue)
                formatter.formatVariableDeclaration(
                  keyword,
                  varName,
                  s"if ($conditionExpr) $thenExpr else $elseExpr",
                  indentLevel
                )
              }.mkString("\n")
            } else {
              val condCode = expressGen.generateExpression(condition)
              val thenCode = generateStatement(thenBranch, indentLevel + 1, analysis)
              val elseCode = generateStatement(elseBranch, indentLevel + 1, analysis)
              formatter.formatIf(condCode, thenCode, Some(elseCode), indentLevel)
            }
          case None =>
            val condCode = expressGen.generateExpression(condition)
            val thenCode = generateStatement(thenBranch, indentLevel + 1, analysis)
            formatter.formatIf(condCode, thenCode, None, indentLevel)
      case transpiler.codegen.ir.IRAugAssignment(_, _, _) => "/* IRAugAssignment: Not implemented Yet */"
      case transpiler.codegen.ir.IRExprStmt(_) => "/* IRExprStmt: Not implemented Yet */"
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

  private def extractAssignmentValue(varName: String, branch: IRNode): IRExpr = branch match {
    case IRBlock(stmts) =>
      stmts.collectFirst {
        case IRAssignment(name, value) if name == varName => value
      }.getOrElse(IRLiteral("null", LiteralType.Null))

    case _ => IRLiteral("null", LiteralType.Null)
  }
}