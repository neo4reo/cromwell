package cwl

import wom.expression.IoFunctionSet
import wom.types._
import wom.values.{WomObject, WomOptionalValue, WomSingleFile, WomString, WomValue}

object ParameterContext {
  val Empty = ParameterContext()
}

case class ParameterContext(inputs: WomValue = WomOptionalValue.none(WomNothingType),
                            self: WomValue =WomOptionalValue.none(WomNothingType),
                            runtime: WomValue = WomOptionalValue.none(WomNothingType)) {
  def withInputs(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet): ParameterContext = {
    val compositeType = WomCompositeType(inputValues map { case (name, value) => name -> value.womType })
    copy(
      inputs = WomObject.withType(
        // TODO: WOM: convert inputValues (including WomFile?) to inputs using the ioFunctionSet
        inputValues map {
          case (name, WomSingleFile(path)) => name -> WomString(path)
          case (name, womValue) => name -> womValue
        },
        compositeType
      )
    )
  }
}
