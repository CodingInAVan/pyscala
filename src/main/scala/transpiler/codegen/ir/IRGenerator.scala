package transpiler.codegen.ir

import generated.Python3Parser
import transpiler.codegen.ir.{BinaryOp, IRBinaryOp, IRExpr, IRLiteral, IRUnaryOp, IRVariable, LiteralType, UnaryOp}

object IRGenerator:
  def convertTextToExpr(text: String): IRExpr = {
    println(s"text = ${text}")
    val trimmed = text.trim

    if (trimmed.contains(" and ")) {
      val parts = trimmed.split(" and ", 2)
      return IRBinaryOp(convertTextToExpr(parts(0)), BinaryOp.And, convertTextToExpr(parts(1)))
    }

    if (trimmed.contains(" or ")) {
      val parts = trimmed.split(" or ", 2)
      return IRBinaryOp(convertTextToExpr(parts(0)), BinaryOp.Or, convertTextToExpr(parts(1)))
    }
    if (trimmed.contains("==")) {
      val parts = trimmed.split("==", 2)
      return IRBinaryOp(convertTextToExpr(parts(0).trim), BinaryOp.Eq, convertTextToExpr(parts(1).trim))
    }
    if (trimmed.contains("!=")) {
      val parts = trimmed.split("!=", 2)
      return IRBinaryOp(convertTextToExpr(parts(0).trim), BinaryOp.Ne, convertTextToExpr(parts(1).trim))
    }
    if (trimmed.contains(">=")) {
      val parts = trimmed.split(">=", 2)
      return IRBinaryOp(convertTextToExpr(parts(0).trim), BinaryOp.Ge, convertTextToExpr(parts(1).trim))
    }
    if (trimmed.contains("<=")) {
      val parts = trimmed.split("<=", 2)
      return IRBinaryOp(convertTextToExpr(parts(0).trim), BinaryOp.Le, convertTextToExpr(parts(1).trim))
    }
    if (trimmed.contains(">")) {
      val parts = trimmed.split(">", 2)
      return IRBinaryOp(convertTextToExpr(parts(0).trim), BinaryOp.Gt, convertTextToExpr(parts(1).trim))
    }
    if (trimmed.contains("<")) {
      val parts = trimmed.split("<", 2)
      return IRBinaryOp(convertTextToExpr(parts(0).trim), BinaryOp.Lt, convertTextToExpr(parts(1).trim))
    }
    if (trimmed.contains("+")) {
      val parts = trimmed.split("\\+", 2)
      if (parts.length == 2) {
        return IRBinaryOp(convertTextToExpr(parts(0).trim), BinaryOp.Add, convertTextToExpr(parts(1).trim))
      }
    }
    if (trimmed.contains("-") && !trimmed.startsWith("-")) {
      val parts = trimmed.split("-", 2)
      if (parts.length == 2) {
        return IRBinaryOp(convertTextToExpr(parts(0).trim), BinaryOp.Sub, convertTextToExpr(parts(1).trim))
      }
    }
    if (trimmed.contains("*")) {
      val parts = trimmed.split("\\*", 2)
      if (parts.length == 2) {
        return IRBinaryOp(convertTextToExpr(parts(0).trim), BinaryOp.Mul, convertTextToExpr(parts(1).trim))
      }
    }
    if (trimmed.contains("/")) {
      val parts = trimmed.split("/", 2)
      if (parts.length == 2) {
        return IRBinaryOp(convertTextToExpr(parts(0).trim), BinaryOp.Div, convertTextToExpr(parts(1).trim))
      }
    }
    // Handle unary operations
    if (trimmed.startsWith("not ")) {
      return IRUnaryOp(UnaryOp.Not, convertTextToExpr(trimmed.substring(4)))
    }

    if (trimmed.startsWith("-")) {
      return IRUnaryOp(UnaryOp.Minus, convertTextToExpr(trimmed.substring(1)))
    }

    // Handle literals and variables
    trimmed match {
      case "True" => IRLiteral("true", LiteralType.Boolean)
      case "False" => IRLiteral("false", LiteralType.Boolean)
      case "None" => IRLiteral("null", LiteralType.Null)
      case s if s.matches("""\d+""") => IRLiteral(s, LiteralType.Integer)
      case s if s.matches("""\d+\.\d+""") => IRLiteral(s, LiteralType.Float)
      case s if s.startsWith("\"") && s.endsWith("\"") => IRLiteral(s.substring(1, s.length - 1), LiteralType.String)
      case s if s.startsWith("'") && s.endsWith("'") =>
        // Convert single quotes to double quotes
        IRLiteral(s.substring(1, s.length - 1), LiteralType.String)
      case s if s.matches("""[a-zA-Z_][a-zA-Z0-9_]*""") => IRVariable(s)
      case _ => IRLiteral(trimmed, LiteralType.String) // Fallback
    }
  }

//  def analyzeIRForMutability(ir: IRNode): Unit = {
//
//  }


