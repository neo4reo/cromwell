package wom.types

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import spray.json.{JsNumber, JsString}
import wom.values.{WomFloat, WomGlobFile, WomSingleDirectory, WomSingleFile, WomString}

import scala.util.Success

class WomFileTypeSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks {

  behavior of "WomFileType"

  lazy val coercionTests = Table(
    ("description", "womFileType", "value", "expected"),

    ("a string to a dir", WomSingleDirectoryType, "example/string", WomSingleDirectory("example/string")),
    ("a string to a file", WomSingleFileType, "example/string", WomSingleFile("example/string")),
    ("a string to a glob", WomGlobFileType, "example/string", WomGlobFile("example/string")),

    ("a js string to a dir", WomSingleDirectoryType, JsString("example/js"), WomSingleDirectory("example/js")),
    ("a js string to a file", WomSingleFileType, JsString("example/js"), WomSingleFile("example/js")),
    ("a js string to a glob", WomGlobFileType, JsString("example/js"), WomGlobFile("example/js")),

    ("a wom string to a dir", WomSingleDirectoryType, WomString("example/wom"), WomSingleDirectory("example/wom")),
    ("a wom string to a file", WomSingleFileType, WomString("example/wom"), WomSingleFile("example/wom")),
    ("a wom string to a glob", WomGlobFileType, WomString("example/wom"), WomGlobFile("example/wom")),

    ("a wom dir to a dir", WomSingleDirectoryType, WomSingleDirectory("example/dir"),
      WomSingleDirectory("example/dir")),
    ("a wom file to a file", WomSingleFileType, WomSingleFile("example/dir"), WomSingleFile("example/dir")),
    ("a wom glob to a glob", WomGlobFileType, WomGlobFile("example/glob*"), WomGlobFile("example/glob*"))
  )

  forAll(coercionTests) { (description, womFileType, value, expected) =>
    it should s"coerce $description" in {
      womFileType.coerceRawValue(value).get should be(expected)
      womFileType.coercionDefined(value) should be(true)
    }
  }

  lazy val failedCoercionTests = Table(
    ("description", "womFileType", "value", "expected"),

    ("a double to a dir", WomSingleDirectoryType, 6.28318,
      "No coercion defined from '6.28318' of type 'java.lang.Double' to 'Dir'."),
    ("a double to a file", WomSingleFileType, 6.28318,
      "No coercion defined from '6.28318' of type 'java.lang.Double' to 'File'."),
    ("a double to a glob", WomGlobFileType, 6.28318,
      "No coercion defined from '6.28318' of type 'java.lang.Double' to 'Glob'."),

    ("a js number to a dir", WomSingleDirectoryType, JsNumber(6.28318),
      "No coercion defined from '6.28318' of type 'spray.json.JsNumber' to 'Dir'."),
    ("a js number to a file", WomSingleFileType, JsNumber(6.28318),
      "No coercion defined from '6.28318' of type 'spray.json.JsNumber' to 'File'."),
    ("a js number to a glob", WomGlobFileType, JsNumber(6.28318),
      "No coercion defined from '6.28318' of type 'spray.json.JsNumber' to 'Glob'."),

    ("a wom float to a dir", WomSingleDirectoryType, WomFloat(6.28318),
      "No coercion defined from '6.28318' of type 'Float' to 'Dir'."),
    ("a wom float to a file", WomSingleFileType, WomFloat(6.28318),
      "No coercion defined from '6.28318' of type 'Float' to 'File'."),
    ("a wom float to a glob", WomGlobFileType, WomFloat(6.28318),
      "No coercion defined from '6.28318' of type 'Float' to 'Glob'."),

    ("a wom dir to a file", WomSingleDirectoryType, WomSingleFile("example/file"),
      """No coercion defined from '"example/file"' of type 'File' to 'Dir'."""),
    ("a wom dir to a glob", WomSingleDirectoryType, WomGlobFile("example/glob*"),
      """No coercion defined from 'glob("example/glob*")' of type 'Glob' to 'Dir'."""),

    ("a wom file to a dir", WomSingleFileType, WomSingleDirectory("example/dir"),
      """No coercion defined from '"example/dir"' of type 'Dir' to 'File'."""),
    ("a wom file to a glob", WomSingleFileType, WomGlobFile("example/glob*"),
      """No coercion defined from 'glob("example/glob*")' of type 'Glob' to 'File'."""),

    ("a wom glob to a dir", WomGlobFileType, WomSingleDirectory("example/dir"),
      """No coercion defined from '"example/dir"' of type 'Dir' to 'Glob'."""),
    ("a wom glob to a file", WomGlobFileType, WomSingleFile("example/file"),
      """No coercion defined from '"example/file"' of type 'File' to 'Glob'.""")
  )

  forAll(failedCoercionTests) { (description, womFileType, value, expected) =>
    it should s"fail to coerce $description" in {
      womFileType.coerceRawValue(value).failed.get.getMessage should be(expected)
      womFileType.coercionDefined(value) should be(false)
    }
  }

  lazy val womFileTypes = Table(
    ("womFileTypeName", "womFileType"),
    ("WomSingleDirectoryType", WomSingleDirectoryType),
    ("WomSingleFileType", WomSingleFileType),
    ("WomGlobFileType", WomGlobFileType)
  )

  forAll(womFileTypes) { (womFileTypeName, womFileType) =>
    it should s"add a $womFileTypeName with a WomStringType" in {
      womFileType.add(WomStringType) should be(Success(womFileType))
      WomStringType.add(womFileType) should be(Success(WomStringType))

      WomOptionalType(womFileType).add(WomOptionalType(WomStringType)) should be(Success(womFileType))
      WomOptionalType(WomStringType).add(WomOptionalType(womFileType)) should be(Success(WomStringType))
    }

    it should s"not add a $womFileTypeName with a WomFileType" in {
      womFileType.add(WomFloatType).failed.get.getMessage should be(
        s"Type evaluation cannot determine type from expression: $womFileTypeName + WomFloatType")
      WomOptionalType(womFileType).add(WomOptionalType(WomFloatType)).failed.get.getMessage should be(
        s"Type evaluation cannot determine type from expression: $womFileTypeName + WomFloatType")
    }

    it should s"equal a $womFileTypeName with a WomStringType" in {
      womFileType.equals(WomStringType) should be(Success(WomBooleanType))
      WomOptionalType(womFileType).equals(WomOptionalType(WomStringType)) should be(Success(WomBooleanType))
    }

    it should s"not compare a $womFileTypeName with a WomFloatType" in {
      womFileType.equals(WomFloatType).failed.get.getMessage should be(
        s"Type evaluation cannot determine type from expression: $womFileTypeName == WomFloatType")
      WomOptionalType(womFileType).equals(WomOptionalType(WomFloatType)).failed.get.getMessage should be(
        s"Type evaluation cannot determine type from expression: $womFileTypeName == WomFloatType")
    }
  }
}
