package cc.hypo.pcd
import scala.util.parsing.combinator.RegexParsers

object Implicits {
  implicit def StringLiteralToString(sl: StringLiteral): String = sl.stringify
  implicit def StringToStringLiteral(s: String): StringLiteral = StringLiteral(s)
}

sealed trait PCDStringifiable {
  def stringify: String
}

sealed trait CropMeasurementLiteral extends PCDStringifiable
// In PDF, scalar should be pt/inch/cm. In PNG/JPG, scalar should be in px.
case class ScalarCropMeasurement(scalar: Double) extends CropMeasurementLiteral 
{
  def stringify = scalar.toString
}

case class RatioCropMeasurement(ratio: Double) extends CropMeasurementLiteral
{
  def stringify = "x" + ratio.toString
}

case class Rect(x: MeasurementLiteral, y: MeasurementLiteral, width: MeasurementLiteral, height: MeasurementLiteral) extends PCDStringifiable
{
  def stringify = s"${x.stringify} ${y.stringify} ${width.stringify} ${height.stringify}"
}
case class CropRect(x: CropMeasurementLiteral, y: CropMeasurementLiteral, width: CropMeasurementLiteral, height: CropMeasurementLiteral) extends PCDStringifiable
{
  def stringify = s"${x.stringify} ${y.stringify} ${width.stringify} ${height.stringify}"
}

sealed trait Rotation
case object ClockwiseRotation extends Rotation
case object CounterClockwiseRotation extends Rotation
case object HalfRotation extends Rotation

sealed trait Color
case class RGBColor(r: Int, g: Int, b: Int) extends Color // Each component from 0-255
case class CMYKAColor(c: Double, m: Double, y: Double, k: Double, a: Double) extends Color // Each component from 0.0 - 1.0
case class NamedColor(colorName: String) extends Color

case class StringLiteral(string: String) extends PCDStringifiable {
  val needToEscapeChars = " \n\t\r\"\\"
  def stringify = 
    if (string == "") "\"\"" // Empty string
    else if (needToEscapeChars.exists(c => string contains c)) 
      "\"" + string.replace("\\", "\\\\").replace("\n", "\\n").replace("\t", "\\t").replace("\r", "\\r").replace("\"", "\\\"") + "\""
    else string
}

sealed abstract trait MeasurementLiteral extends PCDStringifiable {
  def points: Double
  def stringify = points.toString
}
case class PointLiteral(point: Double) extends MeasurementLiteral {
  def points = point
}
case class CentimeterLiteral(cm: Double) extends MeasurementLiteral {
  def points = cm * 72.0 / 2.54
}
case class InchLiteral(inch: Double) extends MeasurementLiteral {
  def points = inch * 72.0
}

sealed trait PCDCommand extends PCDStringifiable
case class BeginPDF(width: MeasurementLiteral, height: MeasurementLiteral) extends PCDCommand {
  def stringify = s"beginpdf ${width.stringify} ${height.stringify}"
}
case class EndPDF(outputPath: StringLiteral) extends PCDCommand {
  def stringify = s"endpdf ${outputPath.stringify}"
}
case class EndJPEGScale(dpi: Double, compression: Double, outputPath: StringLiteral) extends PCDCommand {
  def stringify = s"endjpg $dpi $compression ${outputPath.stringify}"
}
case class EndJPEGSize(width: Int, height: Int, compression: Double, outputPath: StringLiteral) extends PCDCommand {
  def stringify = s"endjpg ${width}px ${height}px $compression ${outputPath.stringify}"
}
case class EndPNGScale(dpi: Double, scale: Double, outputPath: StringLiteral) extends PCDCommand {
  def stringify = s"endpng $dpi $scale ${outputPath.stringify}"
}
case class EndPNGSize(width: Int, height: Int, outputPath: StringLiteral) extends PCDCommand {
  def stringify = s"endpng ${width}px ${height}px ${outputPath.stringify}"
}

case class SimpleImage(url: StringLiteral, frame: Rect, compressionRatio: Option[Double]) extends PCDCommand {
  def stringify = compressionRatio match {
    case Some(compress) => "simpleimage_compress ${url.stringify} $compress ${frame.stringify}"
    case None => "simpleimage ${url.stringify} ${frame.stringify}"
  }
}
case class CropImage(url: StringLiteral, cropRect: CropRect, frame: Rect, compressionRatio: Option[Double]) extends PCDCommand {
  def stringify = compressionRatio match {
    case Some(compress) => "image_compress ${url.stringify} $compress ${cropRect.stringify} ${frame.stringify}"
    case None => "image ${url.stringify} ${cropRect.stringify} ${frame.stringify}"
  }
}

case class UnknownCommand(command: StringLiteral, args: Seq[StringLiteral]) extends PCDCommand {
  def stringify = (command.stringify +: args.map(_.stringify)).mkString(" ")
}

object PCDParser extends RegexParsers {
  override def skipWhitespace = false

  def naturalNumber: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def number: Parser[Double] = """-?\d+(\.\d*)?""".r ^^ { _.toDouble }
  def ptMeasurementLiteral: Parser[PointLiteral] = number ^^ { PointLiteral(_) }
  def cmMeasurementLiteral: Parser[CentimeterLiteral] = number ~ "cm" ^^ { case n ~ "cm" => CentimeterLiteral(n) }
  def inMeasurementLiteral: Parser[InchLiteral] = number ~ "in" ^^ { case n ~ "in" => InchLiteral(n) }


  def measurementLiteral: Parser[MeasurementLiteral] = cmMeasurementLiteral | inMeasurementLiteral | ptMeasurementLiteral

  /** Double quotes (`"`) enclosing a sequence of:
   *
   *  - Any character except double quotes, control characters or backslash (`\`)
   *  - A backslash followed by another backslash, a double quote, or one
   *    of the letters 'n', 't' or 'r'
   */
  def quotedStringLiteral: Parser[StringLiteral] = "\"" ~ rep("""[^"\p{Cntrl}\\]+""".r | """\\["ntr\\]""".r) ~ "\"" ^^ {
    case "\"" ~ chars ~ "\"" => 
      StringLiteral(chars.map({
        case """\\""" => """\"""
        case """\n""" => "\n"
        case """\t""" => "\t"
        case """\r""" => "\r"
        case "\\\"" => "\""
        case c => c
      }).mkString)
  }

  def stringLiteral: Parser[StringLiteral] = quotedStringLiteral | """[^\s#]+""".r ^^ { StringLiteral(_) }

  def sp: Parser[String] = """[ \t]+""".r

  def beginPDF: Parser[BeginPDF] = "beginpdf" ~ sp ~ measurementLiteral ~ sp ~ measurementLiteral ^^ {
    case ("beginpdf" ~ _ ~ width ~ _ ~ height) => BeginPDF(width, height)
  }
  
  def endPDF: Parser[EndPDF] = "endpdf" ~ sp ~ stringLiteral ^^ {
    case ("endpdf" ~ _ ~ path) => EndPDF(path)
  }

  def endJPEGWithSize: Parser[EndJPEGSize] = "endjpg" ~ sp ~ naturalNumber ~ "px" ~ sp ~ naturalNumber ~ "px" ~ sp ~ number ~ sp ~ stringLiteral ^^ {
    case ("endjpg" ~ _ ~ width ~ _ ~ _ ~ height ~ _ ~ _ ~ compression ~ _ ~ path) =>
      EndJPEGSize(width, height, compression, path)
  }

  def endJPEGWithScale: Parser[EndJPEGScale] = "endjpg" ~ sp ~ number ~ sp ~ number ~ sp ~ stringLiteral ^^ {
    case ("endjpg" ~ _ ~ dpi ~ _ ~ compression ~ _ ~ path) =>
      EndJPEGScale(dpi, compression, path)
  }

  def endPNGWithSize: Parser[EndPNGSize] = "endpng" ~ sp ~ naturalNumber ~ "px" ~ sp ~ naturalNumber ~ "px" ~ sp ~ stringLiteral ^^ {
    case ("endpng" ~ _ ~ width ~ "px" ~ _ ~ height ~ "px" ~ _ ~ path) =>
      EndPNGSize(width, height, path)
  }

  def endPNGWithScale: Parser[EndPNGScale] = "endpng" ~ sp ~ number ~ sp ~ number ~ sp ~ stringLiteral ^^ {
    case ("endpng" ~ _ ~ dpi ~ _ ~ scale ~ _ ~ path) =>
      EndPNGScale(dpi, scale, path)
  }

  def rect: Parser[Rect] = measurementLiteral ~ sp ~ measurementLiteral ~ sp ~ measurementLiteral ~ sp ~ measurementLiteral ^^ {
    case (x ~ _ ~ y ~ _ ~ w ~ _ ~ h) => Rect(x, y, w, h)
  }

  def simpleImage: Parser[SimpleImage] = "simpleimage" ~ sp ~ stringLiteral ~ sp ~ rect ^^ {
    case ("simpleimage" ~ _ ~ url ~ _ ~ rect) => SimpleImage(url, rect, None)
  }

  def simpleImageCompress: Parser[SimpleImage] = "simpleimage_compress" ~ sp ~ stringLiteral ~ sp ~ number ~ sp ~ rect ^^ {
    case ("simpleimage_compress" ~ _ ~ url ~ _ ~ compression ~ _ ~ rect) =>
      SimpleImage(url, rect, Some(compression))
  }

  def scalarCropMeasurement: Parser[ScalarCropMeasurement] = measurementLiteral ^^ {
    case m: MeasurementLiteral => ScalarCropMeasurement(m.points)
  }

  def scalarCropPixel: Parser[ScalarCropMeasurement] = naturalNumber ~ "px" ^^ {
    case (d ~ "px") => ScalarCropMeasurement(d)
  }

  def scalarCrop: Parser[ScalarCropMeasurement] = scalarCropPixel | scalarCropMeasurement 

  def ratioCrop: Parser[RatioCropMeasurement] = """[xX\*×]""".r ~ number ^^ {
    case (_ ~ ratio) => RatioCropMeasurement(ratio)
  }

  def cropMeasurement: Parser[CropMeasurementLiteral] = scalarCrop | ratioCrop

  def cropRect: Parser[CropRect] = cropMeasurement ~ sp ~ cropMeasurement ~ sp ~ cropMeasurement ~ sp ~ cropMeasurement ^^ {
    case (x ~ _ ~ y ~ _ ~ w ~ _ ~ h) => CropRect(x, y, w, h)
  }

  def cropImage: Parser[CropImage] = "image" ~ sp ~ stringLiteral ~ sp ~ cropRect ~ sp ~ rect ^^ {
    case ("image" ~ _ ~ url ~ _ ~ cropRect ~ _ ~ frame) =>
      CropImage(url, cropRect, frame, None)
  }

  def cropImageCompress: Parser[CropImage] = "image_compress" ~ sp ~ stringLiteral ~ sp ~ number ~ sp ~ cropRect ~ sp ~ rect ^^ {
    case ("image_compress" ~ _ ~ url ~ _ ~ compression ~ _ ~ cropRect ~ _ ~ frame) => 
      CropImage(url, cropRect, frame, Some(compression))
  }

  def unknownCommand: Parser[UnknownCommand] = stringLiteral ~ rep(sp ~ stringLiteral) ^^ {
    case (cmd ~ args) => 
      UnknownCommand(cmd, args.map{ case (_ ~ arg) => arg })
  }

  def command: Parser[PCDCommand] = beginPDF | endPDF | endJPEGWithSize | endJPEGWithScale | endPNGWithSize | endPNGWithScale | 
                                    simpleImageCompress | simpleImage | cropImageCompress | cropImage | unknownCommand

  def line: Parser[Option[PCDCommand]] = sp.? ~ command.? ~ sp.? ~ "#.*".r.? ^^ {
    case (_ ~ cmd ~ _ ~ comment) => {
      cmd
    }
  }

  def document: Parser[Seq[PCDCommand]] = repsep(line, "\n") ^^ {
    case commands => commands.flatMap(cmdOpt =>
      cmdOpt.map(Seq(_)).getOrElse(Seq())
    )
  }
}