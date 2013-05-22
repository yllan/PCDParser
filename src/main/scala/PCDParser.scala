package models
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

case class StringLiteral(val string: String)

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
  def points = inch * 72
}

sealed trait PCDCommand
case class BeginPDF(width: MeasurementLiteral, height: MeasurementLiteral) extends PCDCommand
case class EndPDF(outputPath: String) extends PCDCommand
case class EndJPEG(width: Int, height: Int, compression: Double, outputPath: String) extends PCDCommand
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
  def number: Parser[Double] = """-?\d+(\.\d*)?""".r ^^ { _.toDouble }
  def ptMeasurementLiteral: Parser[PointLiteral] = number ^^ { PointLiteral(_) }
  def cmMeasurementLiteral: Parser[CentimeterLiteral] = number ~ "cm" ^^ { case n ~ "cm" => CentimeterLiteral(n) }
  def inMeasurementLiteral: Parser[InchLiteral] = number ~ "in" ^^ { case n ~ "in" => InchLiteral(n) }
  def measurementLiteral: Parser[MeasurementLiteral] = cmMeasurementLiteral | inMeasurementLiteral | ptMeasurementLiteral

  // "Double Quote: \", Newline: \n, Tab: \t, Return: \r, Backslash: \\. "
  def quotedStringLiteral: Parser[StringLiteral] = "\"" ~ rep("\\\"" | """\n""" | """\r""" | """\t""" | """\\""" | """[^\\]*""".r) ~ "\"" ^^ {
    case "\"" ~ chars ~ "\"" => StringLiteral(chars.map({
      case "\\\"" => "\""
      case "\\n" => "\n"
      case "\\t" => "\t"
      case "\\r" => "\r"
      case "\\\\" => "\\"
      case c => c
    }).mkString)
  }
  def stringLiteral: Parser[StringLiteral] = quotedStringLiteral | """[^\s]+""".r ^^ { StringLiteral(_) }

}