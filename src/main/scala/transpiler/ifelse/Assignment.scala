package transpiler.ifelse

case class Assignment(variable: String, value: String)
case class IfElseNode(
  condition: String,
  ifAssignments: List[Assignment],
  elseAssignments: List[Assignment]
)
case class Statement(expr: String, assignedVars: Set[String], usedVars: Set[String])
