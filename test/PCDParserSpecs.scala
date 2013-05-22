package test

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers

import models._

class PCDParserSpecs extends Specification with ParserMatchers {
  val parsers = PCDParser

  "number literal" should {
    "recognize integer" in {
      PCDParser.number must succeedOn("5").withResult(5)
      PCDParser.number must succeedOn("-5").withResult(-5)
      PCDParser.number must succeedOn("100").withResult(100)
      PCDParser.number must succeedOn("0").withResult(0)
    }

    "recognize double" in {
      PCDParser.number must succeedOn("0.5").withResult(0.5)
      PCDParser.number must succeedOn("-5.02").withResult(-5.02)
      PCDParser.number must succeedOn("13.599000").withResult(13.599)
    }
  }

  "measurement literal" should {
    "success to recognize cm, pt, inches literal" in {
      PCDParser.measurementLiteral must succeedOn("12").withResult(PointLiteral(12))
      PCDParser.measurementLiteral must succeedOn("30.2cm").withResult(CentimeterLiteral(30.2))
      PCDParser.measurementLiteral must succeedOn("54.30in").withResult(InchLiteral(54.30))
    }
  }
  
  "string literal" should {
    "success to recognize string without whitespaces" in {
      PCDParser.stringLiteral must succeedOn("ABCD").withResult(StringLiteral("ABCD"))
      PCDParser.stringLiteral must succeedOn("""+_*/.,?\""").withResult(StringLiteral("""+_*/.,?\"""))
      PCDParser.stringLiteral must succeedOn("UTF8是中文電波").withResult(StringLiteral("UTF8是中文電波"))

      PCDParser.stringLiteral must succeedOn("two token").partially // only recognize the first token
    }

    "success to recognize quoted string with whitespaces" in {
      PCDParser.stringLiteral must succeedOn("\"two token\"").withResult(StringLiteral("two token"))
    }

    "success to un-escape double quote, newline, tab, return and backslash" in {
      PCDParser.stringLiteral must succeedOn("\"\"").withResult(StringLiteral("")) // empty
      PCDParser.stringLiteral must succeedOn("\"" + """\\""" + "\"").withResult(StringLiteral("\\")) // Backslash
      PCDParser.stringLiteral must succeedOn("\"" + """\n""" + "\"").withResult(StringLiteral("\n")) // Newline
      PCDParser.stringLiteral must succeedOn("\"" + """\t""" + "\"").withResult(StringLiteral("\t")) // Tab
      PCDParser.stringLiteral must succeedOn("\"" + """\r""" + "\"").withResult(StringLiteral("\r")) // Return
      PCDParser.stringLiteral must succeedOn("\"" + """(\"quote\")""" + "\"").withResult(StringLiteral("""("quote")""")) // Double Quote

//       PCDParser.stringLiteral must succeedOn("\"" + """(\"), \\n will translate to \n.""" + "\"").withResult(
//         StringLiteral("""("), \n will translate to
// .""")
//       ) // Mixed
    }

  }
}