package test

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers

import cc.hypo.pcd._

class LiteralParserSpecs extends Specification with ParserMatchers {
  val parsers = LiteralParser

  "number literal" should {
    "recognize integer" in {
      LiteralParser.number must succeedOn("5").withResult(5)
      LiteralParser.number must succeedOn("-5").withResult(-5)
      LiteralParser.number must succeedOn("100").withResult(100)
      LiteralParser.number must succeedOn("0").withResult(0)
    }

    "recognize double" in {
      LiteralParser.number must succeedOn("0.5").withResult(0.5)
      LiteralParser.number must succeedOn("-5.02").withResult(-5.02)
      LiteralParser.number must succeedOn("13.599000").withResult(13.599)
    }
  }

  "measurement literal" should {
    "success to recognize cm, pt, inches literal" in {
      LiteralParser.measurementLiteral must succeedOn("12").withResult(PointLiteral(12))
      LiteralParser.measurementLiteral must succeedOn("30.2cm").withResult(CentimeterLiteral(30.2))
      LiteralParser.measurementLiteral must succeedOn("54.30in").withResult(InchLiteral(54.30))
    }
  }
  
  "string literal" should {
    "success to recognize string without whitespaces" in {
      LiteralParser.stringLiteral must succeedOn("ABCD").withResult(StringLiteral("ABCD"))
      LiteralParser.stringLiteral must succeedOn("""+_*/.,?\""").withResult(StringLiteral("""+_*/.,?\"""))
      LiteralParser.stringLiteral must succeedOn("UTF8是中文電波").withResult(StringLiteral("UTF8是中文電波"))

      LiteralParser.stringLiteral must succeedOn("two token").partially // only recognize the first token
    }

    "success to recognize quoted string with whitespaces" in {
      LiteralParser.stringLiteral must succeedOn("\"two token\"").withResult(StringLiteral("two token"))
    }

    "success to un-escape double quote, newline, tab, return and backslash" in {
      LiteralParser.stringLiteral must succeedOn("\"\"").withResult(StringLiteral("")) // empty
      LiteralParser.stringLiteral must succeedOn("\"" + """\\""" + "\"").withResult(StringLiteral("\\")) // Backslash
      LiteralParser.stringLiteral must succeedOn("\"" + """\n""" + "\"").withResult(StringLiteral("\n")) // Newline
      LiteralParser.stringLiteral must succeedOn("\"" + """\t""" + "\"").withResult(StringLiteral("\t")) // Tab
      LiteralParser.stringLiteral must succeedOn("\"" + """\r""" + "\"").withResult(StringLiteral("\r")) // Return
      LiteralParser.stringLiteral must succeedOn("\"" + """(\"quote\")""" + "\"").withResult(StringLiteral("""("quote")""")) // Double Quote

      LiteralParser.stringLiteral must succeedOn("\"" + """|\", \\n will translate to \n.""" + "\"").withResult(
        StringLiteral("""|", \n will translate to """ + "\n.") 
      ) // Mixed
    }

  }
}