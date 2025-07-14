package transpiler.codegen.ir

sealed trait IRNode
case class IRAssignment(name: String, value: IRExpr) extends IRNode
case class IRAugAssignment(name: String, op: BinaryOp, value: IRExpr) extends IRNode
case class IRIf(cond: IRExpr, thenBranch: IRBlock, elseBranch: Option[IRIf | IRBlock]) extends IRNode
case class IRExprStmt(expr: IRExpr) extends IRNode
case class IRBlock(statements: List[IRNode]) extends IRNode


sealed trait IRExpr
case class IRLiteral(value: String, literalType: LiteralType = LiteralType.String) extends IRExpr
case class IRVariable(name: String) extends IRExpr
case class IRBinaryOp(left: IRExpr, op: BinaryOp, right: IRExpr) extends IRExpr
case class IRUnaryOp(op: UnaryOp, expr: IRExpr) extends IRExpr
case class IRCall(func: IRExpr, args: List[IRExpr]) extends IRExpr

enum LiteralType {
  case String, Integer, Float, Boolean, Null
}

enum BinaryOp(val symbol: String, val pythonSymbol: String = "") {
  case Add extends BinaryOp("+")
  case Sub extends BinaryOp("-")
  case Mul extends BinaryOp("*")
  case Div extends BinaryOp("/")
  case Mode extends BinaryOp("%")
  case IntDiv extends BinaryOp("/", "//")

  case Eq extends BinaryOp("==")
  case Ne extends BinaryOp("!=")
  case Lt extends BinaryOp("<")
  case Le extends BinaryOp("<=")
  case Gt extends BinaryOp(">")
  case Ge extends BinaryOp(">=")
  // Logical
  case And extends BinaryOp("&&", "and")
  case Or extends BinaryOp("||", "or")

  // Bitwise
  case BitAnd extends BinaryOp("&")
  case BitOr extends BinaryOp("|")
  case BitXor extends BinaryOp("^")
}

// Enums for unary operators
enum UnaryOp(val symbol: String, val pythonSymbol: String = "") {
  case Not extends UnaryOp("!", "not")
  case Minus extends UnaryOp("-")
  case Plus extends UnaryOp("+")
  case BitNot extends UnaryOp("~")
}

object UnaryOp {
  // Convert Python operator string to enum
  def fromPython(pythonOp: String): Option[UnaryOp] = pythonOp match {
    case "not" => Some(Not)
    case "-" => Some(Minus)
    case "+" => Some(Plus)
    case "~" => Some(BitNot)
    case _ => None
  }
}