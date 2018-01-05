package cwl

import wom.expression.IoFunctionSet
import wom.types._
import wom.values.{WomMap, WomOptionalValue, WomSingleFile, WomString, WomValue}

object ParameterContext {
  val Empty = ParameterContext()
}

case class ParameterContext(inputs: WomValue = WomOptionalValue(WomNothingType, None),
                            self: WomValue = WomOptionalValue(WomNothingType, None),
                            runtime: WomValue = WomOptionalValue(WomNothingType, None)) {
  def withInputs(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet): ParameterContext = {

    // I'm not 100% sure why the method below turns File into String but since it does we need a special case here:
    val womValueType = WomType.homogeneousTypeFromValues(inputValues.values) match {
      case WomSingleFileType => WomStringType
      case other => other
    }

    copy(
      inputs = WomMap(
        WomMapType(WomStringType, womValueType),
        // TODO: WOM: convert inputValues (including WomFile?) to inputs using the ioFunctionSet
        inputValues map {
          case (name, WomSingleFile(path)) => WomString(name) -> WomString(path)
          case (name, womValue) => WomString(name) -> womValue
        }
      )
    )
  }
}
