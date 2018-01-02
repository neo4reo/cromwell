package cwl

import cats.data.NonEmptyList
import cats.syntax.validated._
import common.validation.ErrorOr.ErrorOr
import common.validation.Validation._
import cwl.InitialWorkDirRequirement.IwdrListingArrayEntry
import wom.expression.{IoFunctionSet, WomExpression}
import wom.types._
import wom.values._

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Try
import cats.syntax.either._

trait CwlWomExpression extends WomExpression {

  def cwlExpressionType: WomType

  override def evaluateType(inputTypes: Map[String, WomType]): ErrorOr[WomType] = cwlExpressionType.validNel
}

case class JobPreparationExpression(expression: Expression,
                                    override val inputs: Set[String]) extends CwlWomExpression {
  val cwlExpressionType = WomAnyType

  override def sourceString = expression match {
    case Expression.ECMAScriptExpression(s) => s.value
    case Expression.ECMAScriptFunction(s) => s.value
  }

  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet) =
    ParameterContext().
      addInputs(inputValues).
      flatMap(pc =>
        expression.
          fold(EvaluateExpression).
            apply(pc).
            toEither.
            leftMap(e => NonEmptyList.one(e.getMessage))
      ).toValidated

  override def evaluateFiles(inputTypes: Map[String, WomValue], ioFunctionSet: IoFunctionSet, coerceTo: WomType) = Set.empty[WomFile].validNel
}

final case class InitialWorkDirFileGeneratorExpression(entry: IwdrListingArrayEntry) extends CwlWomExpression {
  override def cwlExpressionType: WomType = WomSingleFileType
  override def sourceString: String = entry.toString

  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet): ErrorOr[WomValue] = entry match {
    case IwdrListingArrayEntry.StringDirent(content, StringOrExpression.String(entryname), _) =>
      Try(Await.result(ioFunctionSet.writeFile(entryname, content), Duration.Inf)).toErrorOr
    case _ => ??? // TODO WOM and the rest....
  }


  override def evaluateFiles(inputTypes: Map[String, WomValue], ioFunctionSet: IoFunctionSet, coerceTo: WomType): ErrorOr[Set[WomFile]] =
    "Programmer error: Shouldn't use InitialWorkDirRequirement listing to find output files. You silly goose.".invalidNel

  override def inputs: Set[String] = entry match {
    case IwdrListingArrayEntry.StringDirent(_, _, _) => Set.empty
    case _ => Set.empty // TODO WOM: For some cases this might need some...
  }
}

