package transpiler.codegen.scala

import transpiler.codegen.ir.*

trait ExpressionGenerator:
  def generateExpression(expr: IRExpr): String

class ScalaExpressionGenerator extends ExpressionGenerator {

  private val formatter = new ScalaFormatter

  override def generateExpression(expr: IRExpr): String = {
    expr match {
      case IRLiteral(value, _) => value
      case IRVariable(name) => name
      case IRBinaryOp(left, op, right) =>
        val leftCode = generateExpression(left)
        val rightCode = generateExpression(right)
        formatter.formatBinaryOp(leftCode, op.symbol, rightCode)

      case IRUnaryOp(op, operand) =>
        val operandCode = generateExpression(operand)
        formatter.formatUnaryOp(op.symbol, operandCode)

      case IRCall(func, args) =>
        val funcCode = generateExpression(func)
        val argsCode = args.map(generateExpression)
        formatter.formatFunctionCall(funcCode, argsCode)

    }
  }
}
