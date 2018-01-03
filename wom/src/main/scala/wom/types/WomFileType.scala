package wom.types

import spray.json.JsString
import wom.values.{WomGlobFile, WomSingleDirectory, WomSingleFile, WomString}

import scala.util.{Success, Try}

sealed trait WomFileType extends WomPrimitiveType {
  // TODO: WOM: WOMFILE: When WDL supports directories this check may need more work (refactoring to subclasses?)
  override def equals(rhs: WomType): Try[WomType] = rhs match {
    case _: WomFileType => Success(WomBooleanType)
    case WomStringType => Success(WomBooleanType)
    case WomOptionalType(memberType) => equals(memberType)
    case _ => invalid(s"$this == $rhs")
  }
}

sealed trait WomSingleDirectoryOrFileType extends WomFileType

case object WomSingleDirectoryType extends WomSingleDirectoryOrFileType {
  val toDisplayString: String = "Dir"

  override protected def coercion: PartialFunction[Any, WomSingleDirectory] = {
    case s: String => WomSingleDirectory(s)
    case s: JsString => WomSingleDirectory(s.value)
    case s: WomString => WomSingleDirectory(s.valueString)
    case f: WomSingleDirectory => f
  }

  override def add(rhs: WomType): Try[WomType] = rhs match {
    case WomStringType => Success(WomSingleDirectoryType)
    case WomOptionalType(memberType) => add(memberType)
    case _ => invalid(s"$this + $rhs")
  }
}

case object WomSingleFileType extends WomSingleDirectoryOrFileType {
  val toDisplayString: String = "File"

  override protected def coercion: PartialFunction[Any, WomSingleFile] = {
    case s: String => WomSingleFile(s)
    case s: JsString => WomSingleFile(s.value)
    case s: WomString => WomSingleFile(s.valueString)
    case f: WomSingleFile => f
  }

  override def add(rhs: WomType): Try[WomType] = rhs match {
    case WomStringType => Success(WomSingleFileType)
    case WomOptionalType(memberType) => add(memberType)
    case _ => invalid(s"$this + $rhs")
  }
}

case object WomGlobFileType extends WomFileType {
  val toDisplayString: String = "Glob"

  override protected def coercion: PartialFunction[Any, WomGlobFile] = {
    case s: String => WomGlobFile(s)
    case s: JsString => WomGlobFile(s.value)
    case s: WomString => WomGlobFile(s.valueString)
    case f: WomGlobFile => f
  }

  override def add(rhs: WomType): Try[WomType] = rhs match {
    case WomStringType => Success(WomGlobFileType)
    case WomOptionalType(memberType) => add(memberType)
    case _ => invalid(s"$this + $rhs")
  }
}
