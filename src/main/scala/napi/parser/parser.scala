package napi.parser

import fastparse._
import NoWhitespace._

final case class Anno(str: String)
final case class ParamType(anno1: Option[Anno], str: String, pointer: Int, anno2: Option[Anno])
final case class Param(paramType: ParamType, paramName: String)
final case class FuncName(str: String)

sealed trait ParseResult
case object Ignored extends ParseResult
final case class FuncDefinition(returnType: ParamType, name: String, params: Seq[Param]) extends ParseResult

object CommonParser {
  case object PointerType

  def whiteSpace[_: P] = P(CharsWhile(c => c == ' ' || c == '\n'))
  def stringChars(c: Char) = c != '(' && c != ')' && c != ' ' && c != ',' && c != '*' && c != '{' && c != '}' && c != ';' && c != '\n'
  def strChars[_: P] = P(CharsWhile(stringChars).!)
  def comment[_: P] = P("//" ~ CharsWhile(c => c != '\n') ~ "\n")
  def ignore[_: P] = P(whiteSpace | comment)
  def napiExtern[_: P] = P("NAPI_EXTERN")
  def param[_: P] = P(typeDecl ~ strChars).map(Param.tupled)

  def anno[_: P] = P("const".!).map(Anno)
  def pointer[_: P] = P(whiteSpace.? ~ "*".!).map(_ => PointerType)
  def typedef[_: P] = P("typedef")
  def typeDecl[_: P] = P((anno ~ whiteSpace.rep).? ~ strChars.! ~ pointer.rep.? ~ whiteSpace.rep.? ~ anno.? ~ ignore.rep.?).map {
    case (a1, v, p, a2) => ParamType(a1, v, p.map(_.length).getOrElse(0), a2)
  }
}
object FuncParser {
  import CommonParser._
  
  def params[_: P] = P("(" ~ (param ~ "," ~ ignore.rep.?).rep.? ~ param ~ ")" ~ ";").map {
    case (Some(seq), last) => seq ++ Seq(last)
    case (_, param) => Seq(param)
  }

  def funcDecl[_: P] = P(napiExtern.? ~ ignore.rep.? ~ typeDecl ~ strChars.! ~ ignore.rep.? ~ params).map(FuncDefinition.tupled)
}

object TypeParser {
  import CommonParser._
  
  sealed trait TypeDef
  final case class Enum(name: String, elems: Seq[String]) extends TypeDef
  final case class Struct(name: String, params: Seq[Param]) extends TypeDef
  final case class StructAddress(name: String, originType: String) extends TypeDef
  final case class FunctionPointer(name: String, returnType: ParamType, params: Seq[Param]) extends TypeDef
  
  def enumElem[_: P] = P((ignore.rep.? ~ strChars.! ~ "," ~ ignore.rep.?).rep.? ~ ignore.rep.? ~ strChars.! ~ ignore.rep.?).map {
    case (None, elem) => Seq(elem)
    case (Some(seq), elem) => seq ++ Seq(elem)
  }
  def enumDef[_: P] = P(typedef ~ ignore.rep.? ~ "enum" ~ ignore.rep.? ~ "{" ~ enumElem ~ "}" ~ ignore.rep.? ~ strChars.! ~ ignore.rep.? ~ ";").map{ 
    case (elems, name) => Enum(name, elems)
  }

  def structParams[_: P] = P("{" ~ ignore.rep.? ~ (param ~ ";" ~ ignore.rep.?).rep.? ~ "}")
  def struct[_: P] = P(structParams ~ ignore.rep.? ~ strChars.! ~ ";").map{ case (params, name) =>
    Struct(name, params.getOrElse(Seq.empty))
  }
  def structAddress[_: P] = P(strChars.! ~ "*" ~ ignore.rep ~ strChars.! ~ ";").map{ case (origin, name) =>
    StructAddress(name, origin)
  }
  def structKeyword[_: P] = P("struct" ~ ignore.rep.?)
  def structParser[_: P] = P(structKeyword ~ (struct | structAddress))

  def functionPointer[_: P] = P(typeDecl ~ ignore.rep.? ~ "(" ~ "*" ~ strChars.! ~ ")" ~ ignore.rep.? ~ FuncParser.params).map {
    case (paramType, name, params) => FunctionPointer(name, paramType, params)
  }

  def typedefDecl[_: P] = P(typedef ~ ignore.rep ~ (enumDef | structParser | functionPointer))
}

object HeaderParser {
  import CommonParser._

  def statement[_: P] = CharsWhile(c => c != '\n' && c != '\r' && c != '\\')
  def cDefinition[_: P] = P("#" ~ (statement ~ "\\\n").? ~ statement)
  def headerDecl[_: P] = P(
    (((cDefinition ~ "\n") | ignore).rep.? ~ (TypeParser.typedefDecl | FuncParser.funcDecl)).rep.? ~ (cDefinition | ignore).?)
}
