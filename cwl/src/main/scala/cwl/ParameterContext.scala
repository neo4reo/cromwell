package cwl

import wom.types.WomNothingType
import wom.values.{WomOptionalValue, WomValue}

case class ParameterContext(inputs: Map[String, WomValue] = Map.empty,
                            self: WomValue = WomOptionalValue(WomNothingType, None),
                            runtime: Map[String, WomValue] = Map.empty)
