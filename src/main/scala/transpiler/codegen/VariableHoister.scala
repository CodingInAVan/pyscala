package transpiler.codegen

import transpiler.codegen.*
import transpiler.codegen.Variable.VariableInfo

class VariableHoister {
  def generateHoistedDeclarations(variables: Map[String, VariableInfo]): List[String] = {
    val hoistedVars = variables.values
      .filter(_.needsHoisting)
      .toList
      .sortBy(_.name)
    
    hoistedVars.map(generateHoistedDeclaration)
  }
  
  /**
   * Generate a single hoisted declaration
   */
  private def generateHoistedDeclaration(info: VariableInfo): String = {
    val keyword = if (info.isReassigned) "var" else "val"
    val scalaType = inferScalaType(info)
    val initialValue = getDefaultValue(info, scalaType)

    s"$keyword ${info.name}: $scalaType = $initialValue"
  }
  
  private def inferScalaType(info: VariableInfo): String = {
    info.inferredType.getOrElse("Any")
  }
  
  private def getDefaultValue(info: VariableInfo, scalaType: String): String = {
    if (info.isReassigned) {
      // For var, provide safe default that will be overwritten
      scalaType match {
        case "Int" => "0"
        case "String" => "\"\""
        case "Boolean" => "false"
        case "Double" => "0.0"
        case "Float" => "0.0f"
        case "Long" => "0L"
        case _ => "null.asInstanceOf[Any]"
      }
    } else {
      "/* Fix: val needs actual value */"
    }
  }

  def shouldDeclareAsHoisted(variableName: String, variables: Map[String, VariableInfo]): Boolean = {
    variables.get(variableName).exists(_.needsHoisting)
  }
}
