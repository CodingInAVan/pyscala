package transpiler.codegen.ir.analysis

import transpiler.codegen.ir.{IRIf, IRNode, IRVariable}

abstract class IRAnalyzer[T] {
  def analyze(ir: IRNode): Unit
  def getSymbolTable: Map[IRNode, T]
}