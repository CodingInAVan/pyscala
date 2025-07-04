package transpiler.expr

import org.antlr.v4.runtime.ParserRuleContext
import transpiler.codegen.ir.IRNode

class LambdaHandler extends PythonExprHandler {
  override def handle(ctx: ParserRuleContext): Either[String, IRNode] = ???
}
