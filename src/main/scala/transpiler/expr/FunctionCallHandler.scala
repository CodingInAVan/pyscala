package transpiler.expr

import org.antlr.v4.runtime.ParserRuleContext
import transpiler.SymbolTable
import transpiler.codegen.ir.IRNode

class FunctionCallHandler(symbolTable: SymbolTable) extends PythonExprHandler {
  override def handle(ctx: ParserRuleContext): Either[String, IRNode] = ???
}
