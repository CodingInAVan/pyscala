package transpiler

import scala.collection.mutable

class SymbolTable:

  case class VariableInfo(
    name: String,
    isDeclared: Boolean = false,
    isMutable: Boolean = false,
    scalaType: Option[String] = None,
    assignments: Int = 0,
    usedAfter: Boolean = false
  )

  private val scopes = mutable.Stack[mutable.Map[String, VariableInfo]]()

  scopes.push(mutable.Map.empty[String, VariableInfo])

  def enterScope(): Unit = {
    scopes.push(mutable.Map.empty[String, VariableInfo])
  }

  def exitScope(): Unit = {
    if (scopes.size > 1) {
      scopes.pop()
    }
  }

  def isDeclared(name: String): Boolean = {
    scopes.exists(_.contains(name))
  }

  def isMutable(name: String): Boolean = {
    findVariable(name).exists(_.isMutable)
  }

  private def findVariable(name: String): Option[VariableInfo] = {
    scopes.find(_.contains(name)).flatMap(_.get(name))
  }

  def declareVariable(name: String, isMutable: Boolean = false, scalaType: Option[String] = None): Unit = {
    val currentScope = scopes.head
    currentScope(name) = VariableInfo(name, isDeclared = true, isMutable, scalaType)
  }
  
  def updateCurrentScope(varName: String)(update: VariableInfo => VariableInfo): Unit = {
    val currentScope = scopes.head
    val oldInfo = currentScope.getOrElse(varName, VariableInfo(varName, isDeclared = true))
    currentScope(varName) = update(oldInfo)
  }

  /**
  * Mark variable as mutable
  */
  def markMutable(name: String): Unit = {
    scopes.find(_.contains(name)) match {
      case Some(scope) =>
        val info = scope(name)
        scope(name) = info.copy(isMutable = true)
      case None =>
        println(s"Warning: Trying to mark unknown variable '$name' as mutable'")
    }
  }

  /**
  * Handle assignment - returns the appropriate Scala declaration/assignment
  */
  def handleAssignment(name: String, value: String): String = {
    println(s"${name} has been declared? ${isDeclared(name)}")
    if (isDeclared(name)) {
      if (isMutable(name)) {
        s"$name = $value"
      } else {
        markMutable(name)
        s"var $name = $value // Changed from val to var"
      }
    } else {
      // First declaration - assume immutable until proven otherwise
      declareVariable(name, isMutable = false)
      s"val $name = $value"
    }
  }

  /**
  * Analyze all assignments to determine mutability before code generation
  */
  def analyzeAssignments(assignments: List[(String, String)]): Unit = {
    val variableCounts = mutable.Map[String, Int]()

    assignments.foreach { case (name, _) =>
      variableCounts(name) = variableCounts.getOrElseUpdate(name, 0) + 1
    }

    variableCounts.foreach { case (name, count) =>
      if (count > 1) {
        if (isDeclared(name)) {
          markMutable(name)
        } else {
          declareVariable(name, isMutable = true)
        }
      }
    }
  }

  def getCurrentScope: Map[String, VariableInfo] = {
    scopes.head.toMap
  }

  /**
  * Clear all scopes
  */
  def clear(): Unit = {
    scopes.clear()
    scopes.push(mutable.Map.empty[String, VariableInfo])
  }

  def recordAssignment(name: String): Unit = {
    val currentScope = scopes.head
    val updatedInfo = currentScope.get(name) match {
      case Some(info) => info.copy(assignments = info.assignments + 1)
      case None => VariableInfo(name, isDeclared = true, assignments = 1)
    }
    currentScope(name) = updatedInfo
  }

  def recordUsage(name: String): Unit = {
    val infoOpt = findVariable(name)
    infoOpt.foreach { info =>
      val currentScope = scopes.find(_.contains(name)).get
      currentScope(name) = info.copy(usedAfter = true)
    }
  }

  def getHoistableVariables(branches: Set[Set[String]]): Set[String] = {
    if (branches.isEmpty) Set.empty
    else branches.reduce(_ intersect _)
      .filter { name => isUsedAfter(name) }
  }

  def isUsedAfter(name: String): Boolean =
    findVariable(name).exists(_.usedAfter)

  def printSymbolTable(): Unit = {
    println("=== Symbol Table ===")
    scopes.zipWithIndex.reverse.foreach { case (scope, index) =>
      println(s"Scope $index:")
      scope.foreach { case (name, info) =>
        val mutability = if (info.isMutable) "var" else "val"
        println(s"   $name: $mutability ${info.scalaType.getOrElse("Unknown")}")
      }
    }
  }

