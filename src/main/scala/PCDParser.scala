package cc.hypo.pcd
import scala.util.parsing.combinator.RegexParsers

sealed trait CropMeasurementLiteral
// In PDF, scalar should be pt/inch/cm. In PNG/JPG, scalar should be in px.
case class ScalarCropMeasurement(scalar: Double) extends CropMeasurementLiteral 
case class RatioCropMeasurement(ratio: Double) extends CropMeasurementLiteral

case class Rect(x: MeasurementLiteral, y: MeasurementLiteral, width: MeasurementLiteral, height: MeasurementLiteral)
case class CropRect(x: CropMeasurementLiteral, y: CropMeasurementLiteral, width: CropMeasurementLiteral, height: CropMeasurementLiteral)

sealed trait Rotation
case object ClockwiseRotation extends Rotation
case object CounterClockwiseRotation extends Rotation
case object HalfRotation extends Rotation

sealed trait Color
case class RGBColor(r: Int, g: Int, b: Int) extends Color // Each component from 0-255
case class CMYKAColor(c: Double, m: Double, y: Double, k: Double, a: Double) extends Color // Each component from 0.0 - 1.0
case class NamedColor(colorName: String) extends Color

case class StringLiteral(string: String)

sealed abstract trait MeasurementLiteral {
  def points: Double
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

sealed trait PCDCommand
case class BeginPDF(width: MeasurementLiteral, height: MeasurementLiteral) extends PCDCommand
case class EndPDF(outputPath: String) extends PCDCommand
case class EndJPEGScale(dpi: Double, compression: Double, outputPath: String) extends PCDCommand
case class EndJPEGSize(width: Int, height: Int, compression: Double, outputPath: String) extends PCDCommand
case class EndPNGScale(dpi: Double, scale: Double, outputPath: String) extends PCDCommand
case class EndPNGSize(width: Int, height: Int, outputPath: String) extends PCDCommand

/* Set option  */
sealed trait PCDOptionCommand extends PCDCommand
case class SetRadius(radius: MeasurementLiteral) extends PCDOptionCommand
case class SetMaxDPI(maxDPI: Double) extends PCDOptionCommand
case class SetContentRotation(rotation: Rotation) extends PCDOptionCommand
case class SetGeneralOption(key: String, value: String) extends PCDOptionCommand

case class SimpleImage(url: String, frame: Rect, compressionRatio: Option[Double]) extends PCDCommand
case class CropImage(url: String, cropRect: CropRect, frame: Rect, compressionRatio: Option[Double]) extends PCDCommand

case class FillColor(color: Color, frame: Rect) extends PCDCommand

case class DrawText(text: String, frame: Rect) extends PCDCommand

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

  def stringLiteral: Parser[StringLiteral] = quotedStringLiteral | """[^\s]+""".r ^^ { StringLiteral(_) }

  def sp: Parser[String] = """\s+""".r

  def beginPDF: Parser[BeginPDF] = "beginpdf" ~ sp ~ measurementLiteral ~ sp ~ measurementLiteral ^^ {
    case ("beginpdf" ~ _ ~ width ~ _ ~ height) => BeginPDF(width, height)
  }
  
  def endPDF: Parser[EndPDF] = "endpdf" ~ sp ~ stringLiteral ^^ {
    case ("endpdf" ~ _ ~ path) => EndPDF(path.string)
  }

  def endJPEGWithSize: Parser[EndJPEGSize] = "endjpg" ~ sp ~ naturalNumber ~ "px" ~ sp ~ naturalNumber ~ "px" ~ sp ~ number ~ sp ~ stringLiteral ^^ {
    case ("endjpg" ~ _ ~ width ~ _ ~ _ ~ height ~ _ ~ _ ~ compression ~ _ ~ path) =>
      EndJPEGSize(width, height, compression, path.string)
  }

  def endJPEGWithScale: Parser[EndJPEGScale] = "endjpg" ~ sp ~ number ~ sp ~ number ~ sp ~ stringLiteral ^^ {
    case ("endjpg" ~ _ ~ dpi ~ _ ~ compression ~ _ ~ path) =>
      EndJPEGScale(dpi, compression, path.string)
  }

  def endPNGWithSize: Parser[EndPNGSize] = "endpng" ~ sp ~ naturalNumber ~ "px" ~ sp ~ naturalNumber ~ "px" ~ sp ~ stringLiteral ^^ {
    case ("endpng" ~ _ ~ width ~ "px" ~ _ ~ height ~ "px" ~ _ ~ path) =>
      EndPNGSize(width, height, path.string)
  }

  def endPNGWithScale: Parser[EndPNGScale] = "endpng" ~ sp ~ number ~ sp ~ number ~ sp ~ stringLiteral ^^ {
    case ("endpng" ~ _ ~ dpi ~ _ ~ scale ~ _ ~ path) =>
      EndPNGScale(dpi, scale, path.string)
  }

  def rect: Parser[Rect] = measurementLiteral ~ sp ~ measurementLiteral ~ sp ~ measurementLiteral ~ sp ~ measurementLiteral ^^ {
    case (x ~ _ ~ y ~ _ ~ w ~ _ ~ h) => Rect(x, y, w, h)
  }

  def simpleImage: Parser[SimpleImage] = "simpleimage" ~ sp ~ stringLiteral ~ sp ~ rect ^^ {
    case ("simpleimage" ~ _ ~ url ~ _ ~ rect) => SimpleImage(url.string, rect, None)
  }
}