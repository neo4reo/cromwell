package cwl

import cwl.ExpressionEvaluator.{ECMAScriptExpression, ECMAScriptFunction}
import shapeless.Poly1
import wom.values.WomValue

import scala.util.Try

object EvaluateExpression extends Poly1 {
  implicit def script: Case.Aux[ECMAScriptExpression, (ParameterContext, Vector[ECMAScriptFunction]) => Try[WomValue]] = at[ECMAScriptExpression] { e =>
    (parameterContext: ParameterContext, expressionLib: Vector[ECMAScriptFunction]) =>
      ExpressionEvaluator.evalExpression(e, parameterContext)
  }

  implicit def function: Case.Aux[ECMAScriptFunction, (ParameterContext, Vector[ECMAScriptFunction]) => Try[WomValue]] = at[ECMAScriptFunction] { f =>
    (parameterContext: ParameterContext, expressionLib: Vector[ECMAScriptFunction]) =>
      ExpressionEvaluator.evalFunction(f, parameterContext)
  }
}
