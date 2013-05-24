package test

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers

import cc.hypo.pcd._

class PCDParserParserSpecs extends Specification with ParserMatchers {
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

      PCDParser.stringLiteral must succeedOn("\"" + """|\", \\n will translate to \n.""" + "\"").withResult(
        StringLiteral("""|", \n will translate to """ + "\n.") 
      ) // Mixed
    }

  }

  "beginPDF" should {
    "recognize width height" in {
      PCDParser.beginPDF must succeedOn("beginpdf 10.235 23.5").withResult(BeginPDF(PointLiteral(10.235), PointLiteral(23.5)))
      PCDParser.beginPDF must succeedOn("beginpdf 30cm 40cm").withResult(BeginPDF(CentimeterLiteral(30), CentimeterLiteral(40)))
    }

    "failed if wrong number of argument provided" in {
      PCDParser.beginPDF must failOn("beginpdf 10.235")
      PCDParser.beginPDF must failOn("beginpdf 5in 6in 7in")
    }
  }

  "end[PDF/JPEG/PNG]" should {
    "recognize path for endpdf" in {
      PCDParser.endPDF must succeedOn("endpdf file:///tmp/output.pdf").withResult(EndPDF(StringLiteral("file:///tmp/output.pdf")))
      PCDParser.endPDF must succeedOn("endpdf \"file:///tmp/out put.pdf\"").withResult(EndPDF(StringLiteral("file:///tmp/out put.pdf")))
    }

    "recognize width height compression path for endjpg" in {
      PCDParser.endJPEGWithSize must succeedOn("endjpg 500px 300px 0.5 file:///tmp/output.jpg").withResult(EndJPEGSize(500, 300, 0.5, StringLiteral("file:///tmp/output.jpg")))
      PCDParser.endJPEGWithSize must succeedOn("endjpg 500px 300px 0.5 \"file:///tmp/out put.jpg\"").withResult(EndJPEGSize(500, 300, 0.5, StringLiteral("file:///tmp/out put.jpg")))
    }

    "recognize dpi compression path for endjpg" in {
      PCDParser.endJPEGWithScale must succeedOn("endjpg 72 0.5 file:///tmp/output.jpg").withResult(EndJPEGScale(72, 0.5, StringLiteral("file:///tmp/output.jpg")))
    }

    "recognize width height compression path for endpng" in {
      PCDParser.endPNGWithSize must succeedOn("endpng 500px 300px file:///tmp/output.png").withResult(EndPNGSize(500, 300, StringLiteral("file:///tmp/output.png")))
    }

    "recognize dpi scale path for endpng" in {
      PCDParser.endPNGWithScale must succeedOn("endpng 72 2.0 file:///tmp/output.png").withResult(EndPNGScale(72, 2.0, StringLiteral("file:///tmp/output.png")))
    }
  }

  "Image" should {
    "recognize simpleimage url rect" in {
      PCDParser.simpleImage must succeedOn("simpleimage http://localhost/test.png 0.2cm 0.2cm 10cm 10cm").withResult(
        SimpleImage(url = StringLiteral("http://localhost/test.png"), 
                    frame = Rect(CentimeterLiteral(0.2), CentimeterLiteral(0.2), CentimeterLiteral(10), CentimeterLiteral(10)),
                    compressionRatio = None)
      )
    }

    "recognize simpleimage_compress url compression rect" in {
      PCDParser.simpleImageCompress must succeedOn("simpleimage_compress http://localhost/test.png 0.99 0.2cm 0.2cm 10cm 10cm").withResult(
        SimpleImage(url = StringLiteral("http://localhost/test.png"), 
                    frame = Rect(CentimeterLiteral(0.2), CentimeterLiteral(0.2), CentimeterLiteral(10), CentimeterLiteral(10)),
                    compressionRatio = Some(0.99))
      )
    }

    "recognize image url cropRect rect with scalar cropRect" in {
      PCDParser.cropImage must succeedOn("image http://localhost/test.png 0px 10px 200px 200px 0.2cm 0.2cm 10cm 10cm").withResult(
        CropImage(url = StringLiteral("http://localhost/test.png"), 
                  cropRect = CropRect(ScalarCropMeasurement(0), ScalarCropMeasurement(10), ScalarCropMeasurement(200), ScalarCropMeasurement(200)),
                  frame = Rect(CentimeterLiteral(0.2), CentimeterLiteral(0.2), CentimeterLiteral(10), CentimeterLiteral(10)),
                  compressionRatio = None)
      )
    }

    "recognize image url cropRect rect with ratio cropRect" in {
      PCDParser.cropImage must succeedOn("image http://localhost/test.png *0 *0.5 x0.23 ×0.55 0.2cm 0.2cm 10cm 10cm").withResult(
        CropImage(url = StringLiteral("http://localhost/test.png"), 
                  cropRect = CropRect(RatioCropMeasurement(0), RatioCropMeasurement(0.5), RatioCropMeasurement(0.23), RatioCropMeasurement(0.55)),
                  frame = Rect(CentimeterLiteral(0.2), CentimeterLiteral(0.2), CentimeterLiteral(10), CentimeterLiteral(10)),
                  compressionRatio = None)
      )
    }

    "recognize image_compress url compression cropRect rect with scalar cropRect" in {
      PCDParser.cropImageCompress must succeedOn("image_compress http://localhost/test.png 0.97 0px 10px 200px 200px 0.2cm 0.2cm 10cm 10cm").withResult(
        CropImage(url = StringLiteral("http://localhost/test.png"), 
                  cropRect = CropRect(ScalarCropMeasurement(0), ScalarCropMeasurement(10), ScalarCropMeasurement(200), ScalarCropMeasurement(200)),
                  frame = Rect(CentimeterLiteral(0.2), CentimeterLiteral(0.2), CentimeterLiteral(10), CentimeterLiteral(10)),
                  compressionRatio = Some(0.97))
      )
    }

    "recognize image_compress url compression cropRect rect with ratio cropRect" in {
      PCDParser.cropImageCompress must succeedOn("image_compress http://localhost/test.png 0.75 *0 *0.5 x0.23 ×0.55 0.2cm 0.2cm 10cm 10cm").withResult(
        CropImage(url = StringLiteral("http://localhost/test.png"), 
                  cropRect = CropRect(RatioCropMeasurement(0), RatioCropMeasurement(0.5), RatioCropMeasurement(0.23), RatioCropMeasurement(0.55)),
                  frame = Rect(CentimeterLiteral(0.2), CentimeterLiteral(0.2), CentimeterLiteral(10), CentimeterLiteral(10)),
                  compressionRatio = Some(0.75))
      )
    }
  }

  "PCDCommand" should {
    "recognize simpleimage" in {
      PCDParser.command must succeedOn("simpleimage http://localhost/test.png 0.2cm 0.2cm 10cm 10cm").withResult(
        SimpleImage(url = StringLiteral("http://localhost/test.png"), 
                    frame = Rect(CentimeterLiteral(0.2), CentimeterLiteral(0.2), CentimeterLiteral(10), CentimeterLiteral(10)),
                    compressionRatio = None)
      )
    }

    "recognize simpleimage_compress" in {
      PCDParser.command must succeedOn("simpleimage_compress http://localhost/test.png 0.99 0.2cm 0.2cm 10cm 10cm").withResult(
        SimpleImage(url = StringLiteral("http://localhost/test.png"), 
                    frame = Rect(CentimeterLiteral(0.2), CentimeterLiteral(0.2), CentimeterLiteral(10), CentimeterLiteral(10)),
                    compressionRatio = Some(0.99))
      )
    }

    "recognize image" in {
      PCDParser.command must succeedOn("image http://localhost/test.png 0px 10px 200px 200px 0.2cm 0.2cm 10cm 10cm").withResult(
        CropImage(url = StringLiteral("http://localhost/test.png"), 
                  cropRect = CropRect(ScalarCropMeasurement(0), ScalarCropMeasurement(10), ScalarCropMeasurement(200), ScalarCropMeasurement(200)),
                  frame = Rect(CentimeterLiteral(0.2), CentimeterLiteral(0.2), CentimeterLiteral(10), CentimeterLiteral(10)),
                  compressionRatio = None)
      )

      PCDParser.command must succeedOn("image http://localhost/test.png *0 *0.5 x0.23 ×0.55 0.2cm 0.2cm 10cm 10cm").withResult(
        CropImage(url = StringLiteral("http://localhost/test.png"), 
                  cropRect = CropRect(RatioCropMeasurement(0), RatioCropMeasurement(0.5), RatioCropMeasurement(0.23), RatioCropMeasurement(0.55)),
                  frame = Rect(CentimeterLiteral(0.2), CentimeterLiteral(0.2), CentimeterLiteral(10), CentimeterLiteral(10)),
                  compressionRatio = None)
      )
    }

    "recognize image_compress" in {
      PCDParser.command must succeedOn("image_compress http://localhost/test.png 0.97 0px 10px 200px 200px 0.2cm 0.2cm 10cm 10cm").withResult(
        CropImage(url = StringLiteral("http://localhost/test.png"), 
                  cropRect = CropRect(ScalarCropMeasurement(0), ScalarCropMeasurement(10), ScalarCropMeasurement(200), ScalarCropMeasurement(200)),
                  frame = Rect(CentimeterLiteral(0.2), CentimeterLiteral(0.2), CentimeterLiteral(10), CentimeterLiteral(10)),
                  compressionRatio = Some(0.97))
      )
      PCDParser.command must succeedOn("image_compress http://localhost/test.png 0.75 *0 *0.5 x0.23 ×0.55 0.2cm 0.2cm 10cm 10cm").withResult(
        CropImage(url = StringLiteral("http://localhost/test.png"), 
                  cropRect = CropRect(RatioCropMeasurement(0), RatioCropMeasurement(0.5), RatioCropMeasurement(0.23), RatioCropMeasurement(0.55)),
                  frame = Rect(CentimeterLiteral(0.2), CentimeterLiteral(0.2), CentimeterLiteral(10), CentimeterLiteral(10)),
                  compressionRatio = Some(0.75))
      )
    }

    "recognize endpdf/endjpg/endpng" in {
      PCDParser.command must succeedOn("endpdf file:///tmp/output.pdf").withResult(EndPDF(StringLiteral("file:///tmp/output.pdf")))
      PCDParser.command must succeedOn("endpdf \"file:///tmp/out put.pdf\"").withResult(EndPDF(StringLiteral("file:///tmp/out put.pdf")))
      PCDParser.command must succeedOn("endjpg 500px 300px 0.5 file:///tmp/output.jpg").withResult(EndJPEGSize(500, 300, 0.5, StringLiteral("file:///tmp/output.jpg")))
      PCDParser.command must succeedOn("endjpg 500px 300px 0.5 \"file:///tmp/out put.jpg\"").withResult(EndJPEGSize(500, 300, 0.5, StringLiteral("file:///tmp/out put.jpg")))
      PCDParser.command must succeedOn("endjpg 72 0.5 file:///tmp/output.jpg").withResult(EndJPEGScale(72, 0.5, StringLiteral("file:///tmp/output.jpg")))
      PCDParser.command must succeedOn("endpng 500px 300px file:///tmp/output.png").withResult(EndPNGSize(500, 300, StringLiteral("file:///tmp/output.png")))
      PCDParser.command must succeedOn("endpng 72 2.0 file:///tmp/output.png").withResult(EndPNGScale(72, 2.0, StringLiteral("file:///tmp/output.png")))
    }

    "recognize beginpdf" in {
      PCDParser.command must succeedOn("beginpdf 10.235 23.5").withResult(BeginPDF(PointLiteral(10.235), PointLiteral(23.5)))
      PCDParser.command must succeedOn("beginpdf 30cm 40cm").withResult(BeginPDF(CentimeterLiteral(30), CentimeterLiteral(40)))
    }

    "recognize any command" in {
      PCDParser.command must succeedOn("set Color 0xCAFEBB").withResult(UnknownCommand(StringLiteral("set"), Seq(StringLiteral("Color"), StringLiteral("0xCAFEBB"))))

      PCDParser.command must succeedOn("text 5cm 5cm 10cm 10cm \"Hello, world\"").withResult(
        UnknownCommand(StringLiteral("text"), Seq(StringLiteral("5cm"), StringLiteral("5cm"), StringLiteral("10cm"), StringLiteral("10cm"), StringLiteral("Hello, world")))
      )
    }
  }

  "PCDDocument" should {
    "parse line" in {
      PCDParser.line must succeedOn("").withResult(None)
      PCDParser.line must succeedOn("beginpdf 10 10 # with comment").withResult(Some(
        BeginPDF(PointLiteral(10), PointLiteral(10))
      ))
      PCDParser.line must succeedOn(" beginpdf  10 10 # has indent").withResult(Some(
        BeginPDF(PointLiteral(10), PointLiteral(10))
      ))
    }

    "parse a full pcd file" in {
      val pcd = """

      # This is a pcd file
beginpdf 30cm 40cm # size

  simpleimage http://www.apple.com/apple.png 0.5cm 0.5cm 3cm 3cm""" +
"\n\t" + """set Color CMYK:0.8,0.7,0.5,0.0
  text 5 5 100 200 "cool\\\"fun\" sentence"

endpdf  file:///tmp/output.pdf
      """

      PCDParser.document must succeedOn(pcd).withResult(Seq(
        BeginPDF(CentimeterLiteral(30), CentimeterLiteral(40)),
        SimpleImage(StringLiteral("http://www.apple.com/apple.png"), Rect(CentimeterLiteral(0.5), CentimeterLiteral(0.5), CentimeterLiteral(3), CentimeterLiteral(3)), None),
        UnknownCommand(StringLiteral("set"), Seq(StringLiteral("Color"), StringLiteral("CMYK:0.8,0.7,0.5,0.0"))),
        UnknownCommand(StringLiteral("text"), Seq(StringLiteral("5"), StringLiteral("5"), StringLiteral("100"), StringLiteral("200"), StringLiteral("""cool\"fun" sentence"""))),
        EndPDF(StringLiteral("file:///tmp/output.pdf"))
      ))

    }
  }

  "Stringify" should {
    "Parse back to equal structure if it contains no cm/inch literal" in {
      val commands = Seq(
        BeginPDF(PointLiteral(30), PointLiteral(40)),
        SimpleImage(StringLiteral("http://www.apple.com/apple.png"), Rect(PointLiteral(0.5), PointLiteral(0.5), PointLiteral(3), PointLiteral(3)), None),
        SimpleImage(StringLiteral(""), Rect(PointLiteral(0.5), PointLiteral(0.5), PointLiteral(3), PointLiteral(3)), None),
        UnknownCommand(StringLiteral("set"), Seq(StringLiteral("Color"), StringLiteral("CMYK:0.8,0.7,0.5,0.0"))),
        UnknownCommand(StringLiteral("text"), Seq(StringLiteral("5"), StringLiteral("5"), StringLiteral("100"), StringLiteral("200"), StringLiteral("""cool\"fun" sentence"""))),
        EndPDF(StringLiteral("file:///tmp/output.pdf")),
        EndPNGScale(72, 2.0, StringLiteral("file:///tmp/output.png")),
        EndPNGSize(500, 300, StringLiteral("file:///tmp/output.png")),
        EndJPEGSize(500, 300, 0.5, StringLiteral("file:///tmp/output.jpg")),
        EndJPEGScale(72, 0.5, StringLiteral("file:///tmp/output.jpg")),
        SimpleImage(url = StringLiteral("http://localhost/test.png"), 
                    frame = Rect(PointLiteral(0.2), PointLiteral(0.2), PointLiteral(10), PointLiteral(10)),
                    compressionRatio = Some(0.99)),
        SimpleImage(url = StringLiteral("http://localhost/test.png"), 
                    frame = Rect(PointLiteral(0.2), PointLiteral(0.2), PointLiteral(10), PointLiteral(10)),
                    compressionRatio = None),
        CropImage(url = StringLiteral("http://localhost/test.png"), 
                  cropRect = CropRect(ScalarCropMeasurement(0), ScalarCropMeasurement(10), ScalarCropMeasurement(200), ScalarCropMeasurement(200)),
                  frame = Rect(PointLiteral(0.2), PointLiteral(0.2), PointLiteral(10), PointLiteral(10)),
                  compressionRatio = Some(0.97)),
        CropImage(url = StringLiteral("http://localhost/test.png"), 
                  cropRect = CropRect(RatioCropMeasurement(0), RatioCropMeasurement(0.5), RatioCropMeasurement(0.23), RatioCropMeasurement(0.55)),
                  frame = Rect(PointLiteral(0.2), PointLiteral(0.2), PointLiteral(10), PointLiteral(10)),
                  compressionRatio = Some(0.75))

      )
      PCDParser.document must succeedOn(commands.map(_.stringify).mkString("\n")).withResult(commands)
    }

  }

}