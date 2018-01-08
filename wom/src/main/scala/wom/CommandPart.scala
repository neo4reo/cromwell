package wom

import java.util.Collections

import common.validation.ErrorOr.ErrorOr
import org.apache.commons.text.translate.LookupTranslator
import wom.callable.RuntimeEnvironment
import wom.expression.IoFunctionSet
import wom.graph.LocalName
import wom.values.WomValue

trait CommandPart {
  def instantiate(inputsMap: Map[LocalName, WomValue],
                  functions: IoFunctionSet,
                  valueMapper: WomValue => WomValue,
                  runtimeEnvironment: RuntimeEnvironment): ErrorOr[InstantiatedCommand]
}

object CommandPart {

  val DoubleQuoteEscaper = {
    val map = new java.util.HashMap[String, String]()
    map.put("\"", "\\\"")
    new LookupTranslator(Collections.unmodifiableMap(map))
  }

  implicit class ShellQuoteHelper(val string: String) extends AnyVal {
    def quoteForShell: String = {
      '"' + DoubleQuoteEscaper.translate(string) + '"'
    }
  }
}
