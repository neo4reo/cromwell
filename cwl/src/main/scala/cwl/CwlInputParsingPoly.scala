package cwl

import cats.syntax.validated._
import common.validation.ErrorOr.ErrorOr
import common.validation.Validation._
import shapeless.Poly1
import wom.executable.Executable.DelayedCoercionFunction
import wom.types.{WomArrayType, WomCompositeType, WomSingleFileType, WomType}
import wom.values._

private [cwl] object CwlInputCoercion extends Poly1 {
  implicit def cwlFileToWomValue: Case.Aux[MyriadInputValuePrimitives, DelayedCoercionFunction] = at[MyriadInputValuePrimitives] {
    _.fold(CwlInputPrimitiveCoercion)
  }
  
  implicit def inputArrayValueToWomValue: Case.Aux[Array[MyriadInputValuePrimitives], DelayedCoercionFunction] =
    at[Array[MyriadInputValuePrimitives]] { arrayValue =>
      womType: WomType => {
        import cats.instances.list._
        import cats.syntax.traverse._

        womType match {
          case womArrayType: WomArrayType =>
            arrayValue.toList
              .traverse[ErrorOr, WomValue](_.fold(CwlInputPrimitiveCoercion).apply(womArrayType.memberType))
              .map { WomArray(womArrayType, _) }

          case other => s"Cannot convert an array input value into a non array type: $other".invalidNel
        }
      }
    }

  implicit def inputMapValueToWomValue: Case.Aux[Map[String, MyriadInputValuePrimitives], DelayedCoercionFunction] =
    at[Map[String, MyriadInputValuePrimitives]] { valueMap =>
      womType: WomType => {
        import cats.instances.list._
        import cats.syntax.traverse._

        womType match {
          case compositeType @ WomCompositeType(typeMap) =>
            valueMap.toList
              .traverse[ErrorOr, (String, WomValue)]({
              case (key, value) =>
                typeMap.get(key) match {
                  case Some(expectedWomType) => value.fold(CwlInputPrimitiveCoercion).apply(expectedWomType) map { key -> _ }
                  case None => s"Undeclared field $key found in input values".invalidNel
                }
            }).map { result => WomObject.withType(result.toMap, compositeType) }

          case other => s"Cannot convert a map input value into a non record schema type: $other".invalidNel
        }
      }
    }
}

private [cwl] object CwlInputPrimitiveCoercion extends Poly1 {
  private def simpleCoercion[T](value: T)(womType: WomType) = {
    womType.coerceRawValue(value).toErrorOr
  }
  
  implicit def cwlFileToWomValue: Case.Aux[File, DelayedCoercionFunction] = at[File] { cwlFile =>
    womType: WomType => {
      womType match {
        case WomSingleFileType => cwlFile.asWomValue
        case otherType => s"Input value is a File but the targeted input is a $otherType".invalidNel
      }
    }
  }

  implicit def stringToWomValue: Case.Aux[String, DelayedCoercionFunction] = at[String](simpleCoercion)
  implicit def booleanToWomValue: Case.Aux[Boolean, DelayedCoercionFunction] = at[Boolean](simpleCoercion)
  implicit def intToWomValue: Case.Aux[Int, DelayedCoercionFunction] = at[Int](simpleCoercion)
  implicit def floatToWomValue: Case.Aux[Float, DelayedCoercionFunction] = at[Float](simpleCoercion)
  implicit def doubleToWomValue: Case.Aux[Double, DelayedCoercionFunction] = at[Double](simpleCoercion)
  implicit def longToWomValue: Case.Aux[Long, DelayedCoercionFunction] = at[Long](simpleCoercion)
}
