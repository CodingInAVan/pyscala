package transpiler.expr

import org.antlr.v4.runtime.ParserRuleContext
import transpiler.ir.IRNode

trait PythonExprHandler:
  def handle(ctx: ParserRuleContext): Either[String, IRNode]