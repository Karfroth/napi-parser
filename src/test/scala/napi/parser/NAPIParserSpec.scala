package napi.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import napi.parser._
import fastparse._
import MultiLineWhitespace._
import napi.parser.TypeParser.Struct
import napi.parser.TypeParser.StructAddress
import napi.parser.TypeParser.FunctionPointer

class NAPIParserSpec extends AnyFlatSpec with Matchers {
  "Func Parser" should "parse as expected" in {

    def testStr =
    """
      |NAPI_EXTERN napi_status napi_throw_error (napi_env env,
      |const char** utf8name,
      |napi_value* const result);
    """.stripMargin.trim()
    def parseFunc[_: P] = P(FuncParser.funcDecl ~ End)
    def res = parse(testStr, parseFunc(_))
    val returnType = ParamType(None, "napi_status", 0, None)
    val funcName = "napi_throw_error"
    val params = Seq(
      Param(
        ParamType(None, "napi_env", 0, None)
      , "env"
      )
    , Param(
        ParamType(Some(Anno("const")), "char", 2, None)
      , "utf8name"
      )
    , Param(
        ParamType(None, "napi_value", 1, Some(Anno("const")))
      , "result"
      )
    )
    val expected = FuncDefinition(
      returnType
    , funcName
    , params
    )
    res shouldEqual Parsed.Success(expected, 105)
  }
  "TypeParser enumDef" should "parse enum definition" in {
    def testStr = 
      """
        |typedef enum {
        |  napi_a,
        |  napi_b
        |} napi_ab;
      """.stripMargin.trim

    def parser[_: P] = P(TypeParser.enumDef ~ End)
    def res = parse(testStr, parser(_))
    def expected = Parsed.Success(
      TypeParser.Enum(
        "napi_ab"
      , List("napi_a", "napi_b")
      )
    , 44)
    println(res)
    println(expected)
    res shouldEqual expected
  }
  "Type Parser typedefDecl" should "parse struct" in {
    def testStr = 
      """
        |typedef struct {
        |  a aa;
        |  b bb;
        |  c cc;
        |} napi_abc;
      """.stripMargin.trim
    def parser[_: P] = P(TypeParser.typedefDecl ~ End)
    def res = parse(testStr, parser(_))
    def expected = Parsed.Success(Struct(
      "napi_abc"
    , Seq(
      Param(ParamType(None, "a", 0, None), "aa")
    , Param(ParamType(None, "b", 0, None), "bb")
    , Param(ParamType(None, "c", 0, None), "cc")
    )
    ), 52)
    res shouldEqual expected
  }
  "Type Parser typedefDecl" should "parse structAddress" in {
    def testStr = 
      """
        |typedef struct napi_aaa__* napi_aaa;
      """.stripMargin.trim
    def parser[_: P] = P(TypeParser.typedefDecl ~ End)
    def res = parse(testStr, parser(_))
    def expected = Parsed.Success(StructAddress("napi_aaa", "napi_aaa__"), 36)
    res shouldEqual expected
  }
  "Type Parser typedefDecl" should "parse function pointer" in {
    def testStr =
      """
        |typedef void (*napi_name)(napi_a a,
        |                          void* b);
      """.stripMargin.trim
    def parser[_: P] = P(TypeParser.typedefDecl ~ End)
    def res = parse(testStr, parser(_))
    def expected = Parsed.Success(
      FunctionPointer(
        "napi_name"
      , ParamType(None, "void", 0, None)
      , Seq(
          Param(ParamType(None, "napi_a", 0, None), "a")
        , Param(ParamType(None, "void", 1, None), "b")
        )
      )
    , 71)
    res shouldEqual expected
  }

  "Header Parser headerDecl" should "parse sample header" in {
    def testStr = scala.io.Source.fromResource("c.h").getLines.mkString("\n").stripMargin.trim

    def parser[_: P] = P(HeaderParser.headerDecl ~ End)
    def res = parse(testStr, parser(_))
    def expected = Parsed.Success(
      Some(
        List(
          FuncDefinition(
            ParamType(None, "some_type", 0, None)
          , "napi_func"
          , List(
              Param(
                ParamType(None, "napi_env", 0, None)
              , "env"
              )
            )
          )
        , Struct(
            "napi_abc"
          , Seq(
              Param(ParamType(None, "a", 0, None), "aa")
            , Param(ParamType(None, "b", 0, None), "bb")
            , Param(ParamType(None, "c", 0, None), "cc")
            )
          )
        , StructAddress("napi_aaa", "napi_aaa__")
        , Struct(
            "napi_abc2"
          , Seq(
              Param(ParamType(None, "a", 0, None), "aa")
            , Param(ParamType(None, "b", 1, None), "bb")
            , Param(ParamType(None, "c", 0, None), "cc")
            )
          )
        , FunctionPointer(
            "napi_name"
          , ParamType(None, "void", 0, None)
          , Seq(
              Param(ParamType(None, "napi_a", 0, None), "a")
            , Param(ParamType(None, "void", 1, None), "b")
            )
          )
        , FunctionPointer(
            "napi_name2"
          , ParamType(None, "int32_t", 1, None)
          , Seq(
              Param(ParamType(None, "napi_a", 1, None), "a2")
            , Param(ParamType(None, "void", 2, None), "b")
            )
          )
        )
      )
    , 622)
    res shouldEqual expected 
  }
}
