package transpiler.codegen.scala

import transpiler.codegen.Variable.{BlockId, VariableInfo}
import transpiler.codegen.ir.*
import transpiler.codegen.scala.analyzer.{CombinedAnalysisResult, IfAssignmentAnalyzer}

import scala.collection.mutable
class VariableAnalyzer:

  private val variableInfo = mutable.Map[String, VariableInfo]()
  private val blockScopes = mutable.Map[BlockId, Set[String]]()
  private var blockIdCounter = 0
  private val ifAssignmentAnalyzer = new IfAssignmentAnalyzer(variableInfo)

  /**
  * Analyze variable usage in the given IR
  */
  def analyze(ir: IRNode): CombinedAnalysisResult = {
    reset()
    analyzeNode(ir, "main")
    determineHoisting()
    CombinedAnalysisResult(variableInfo.toMap, blockScopes.toMap, ifAssignmentAnalyzer.analyze(ir))
  }

  private def reset(): Unit = {
    variableInfo.clear()
    blockScopes.clear()
    blockIdCounter = 0
  }
  private def nextBlockId(): BlockId = {
    blockIdCounter += 1
    s"block_${blockIdCounter}"
  }

  private def analyzeNode(ir: IRNode, currentBlockId: BlockId): Unit = {
    ir match {
      case IRBlock(statements) =>
        initializeBlock(currentBlockId)
        statements.foreach(analyzeNode(_, currentBlockId))

      case IRAssignment(name, value) =>
        recordDefinition(name, currentBlockId, value)
        analyzeExpression(value, currentBlockId)

      case IRReAssignment(name, value) =>
        recordReassignment(name, currentBlockId)
        analyzeExpression(value, currentBlockId)

      case IRIf(condition, thenBranch, elseBranch) =>
        analyzeExpression(condition, currentBlockId)

        val thenBlockId = nextBlockId()
        analyzeNode(thenBranch, thenBlockId)

        elseBranch.foreach {
          case elseIf: IRIf =>
            analyzeNode(elseIf, currentBlockId)
          case elseBlock: IRBlock =>
            val elseBlockId = nextBlockId()
            analyzeNode(elseBlock, elseBlockId)
        }

      case IRExprStmt(expr) =>
        analyzeExpression(expr, currentBlockId)
    }
  }
  private def analyzeExpression(expr: IRExpr, currentBlockId: BlockId): Unit = {
    expr match {
      case IRVariable(name) =>
        recordUsage(name, currentBlockId)

      case IRBinaryOp(left, _, right) =>
        analyzeExpression(left, currentBlockId)
        analyzeExpression(right, currentBlockId)

      case IRUnaryOp(_, operand) =>
        analyzeExpression(operand, currentBlockId)

      case IRCall(func, args) =>
        analyzeExpression(func, currentBlockId)
        args.foreach(analyzeExpression(_, currentBlockId))

      case IRLiteral(_, _) =>
    }
  }
  private def initializeBlock(blockId: BlockId): Unit = {
    blockScopes(blockId) = Set.empty
  }
  private def recordDefinition(name: String, blockId: BlockId, value: IRExpr): Unit = {
    println(s"recordDefinition, name = ${name}")
    val current = variableInfo.getOrElse(name, VariableInfo(name))
    if (variableInfo.contains(name) && current.definedInBlocks.contains(blockId)) {
      variableInfo(name) = current.copy (
        definedInBlocks = current.definedInBlocks + blockId,
        isReassigned = true,
        assignedValues = current.assignedValues :+ value
      )
    } else {
      variableInfo(name) = current.copy(
        definedInBlocks = current.definedInBlocks + blockId,
        assignedValues = current.assignedValues :+ value
      )
    }
    val info = variableInfo(name)
    val fields = info.productElementNames.zip(info.productIterator)
    val str = fields.map{ case (name, value) => s"$name=$value" }.mkString(", ")
    println(s"VariableInfo($str)")

    val currentScope = blockScopes.getOrElse(blockId, Set.empty)
    blockScopes(blockId) = currentScope + name
  }

  private def recordReassignment(name: String, blockId: BlockId): Unit = {
    val current = variableInfo.getOrElse(name, VariableInfo(name))
    variableInfo(name) = current.copy(
      isReassigned = true,
      usedInBlocks = current.usedInBlocks + blockId
    )
  }
  private def recordUsage(name: String, blockId: BlockId): Unit = {
    val current = variableInfo.getOrElse(name, VariableInfo(name))
    variableInfo(name) = current.copy(
      usedInBlocks = current.usedInBlocks + blockId
    )
  }

  private def determineHoisting(): Unit = {
    val assignmentCounts = mutable.Map[String, Int]()

    variableInfo.foreach { case (name, info) =>
      assignmentCounts(name) = info.definedInBlocks.size

      val inferredType = inferTypeFromValues(info.assignedValues)
      variableInfo(name) = info.copy(inferredType = Some(inferredType))
    }

    variableInfo.foreach { case (name, info) =>
      val needsHoisting = shouldHoist(info)
      variableInfo(name) = info.copy(needsHoisting = needsHoisting)
    }
  }

  private def shouldHoist(info: VariableInfo): Boolean = {
    // Case 1: Variable defined in conditional blocks but used in main/parent scope
    if (info.definedInBlocks.exists(!_.equals("main")) && info.usedInBlocks.contains("main")) {
      println(s"  ${info.name} needs hoisting: defined in conditional but used in main scope")
      return true
    }

    // Case 2: Variable defined in multiple blocks AND used outside those blocks
    if (info.definedInBlocks.size > 1) {
      val hasUsageOutsideDefinitionBlocks = info.usedInBlocks.exists(!info.definedInBlocks.contains(_))
      if (hasUsageOutsideDefinitionBlocks) {
        println(s"  ${info.name} needs hoisting: defined in multiple blocks and used outside them")
        return true
      } else {
        println(s"  ${info.name} could use if-expression: defined in multiple blocks but only used within them")
        return false
      }
    }

    if (info.isReassigned && info.definedInBlocks.size == 1) {
      println(s"  ${info.name} needs hoisting: reassigned within same block (needs var)")
      return true
    }

    false
  }

  private def inferTypeFromValues(values: List[IRExpr]): String = {
    if (values.isEmpty) return "Any"

    val types = values.collect {
      case IRLiteral(_, literalType) => scalaTypeFromLiteralType(literalType)
    }

    if (types.isEmpty) {
      "Any"
    } else if (types.toSet.size == 1) {
      types.head
    } else {
      findCommonType(types.toSet)
    }
  }

  private def findCommonType(types: Set[String]): String = {
    if (types.contains("String")) {
      "Any"
    } else if (types.subsetOf(Set("Int", "Double"))) {
      "Double"
    } else if (types.subsetOf(Set("Boolean"))) {
      "Boolean"
    } else {
      "Any"
    }
  }

  private def scalaTypeFromLiteralType(literalType: LiteralType): String = {
    literalType match {
      case LiteralType.Integer => "Int"
      case LiteralType.Float => "Double"
      case LiteralType.String => "String"
      case LiteralType.Boolean => "Boolean"
      case LiteralType.Null => "Any" // null can be any reference type
    }
  }
