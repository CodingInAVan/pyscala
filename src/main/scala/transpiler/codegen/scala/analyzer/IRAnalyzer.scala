package transpiler.codegen.scala.analyzer

import transpiler.codegen.Variable.{BlockId, VariableInfo}
import transpiler.codegen.ir.IRNode

trait IRAnalyzer:
  def analyze(ir: IRNode): AnalyzerResult

trait AnalyzerResult

case class IfAssignmentResult(varNames: Set[String]) extends AnalyzerResult

case class CombinedAnalysisResult(
  variables: Map[String, VariableInfo],
  blockScopes: Map[BlockId, Set[String]],
  ifAssignmentResult: AnalyzerResult
)