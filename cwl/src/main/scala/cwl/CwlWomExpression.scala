package cwl

import cats.syntax.option._
import common.validation.ErrorOr._
import common.validation.Validation._
import common.validation.ErrorOr.ShortCircuitingFlatMap
import cats.syntax.validated._
import cwl.InitialWorkDirRequirement.IwdrListingArrayEntry
import cwl.WorkflowStepInput.InputSource
import cwl.command.ParentName
import wom.expression.{IoFunctionSet, WomExpression}
import wom.types._
import wom.values._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

sealed trait CwlWomExpression extends WomExpression {

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

  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet) = {
    val pc = ParameterContext(inputValues)
    ExpressionEvaluator.eval(expression, pc)
  }

  override def evaluateFiles(inputTypes: Map[String, WomValue], ioFunctionSet: IoFunctionSet, coerceTo: WomType) = Set.empty[WomFile].validNel
}

case class CommandOutputExpression
(
  outputBinding: CommandOutputBinding,
  override val cwlExpressionType: WomType,
  override val inputs: Set[String],
  secondaryFilesCoproduct: Option[SecondaryFiles] = None,
  formatCoproduct: Option[StringOrExpression] = None //only valid when type: File
) extends CwlWomExpression {

  // TODO WOM: outputBinding.toString is probably not be the best representation of the outputBinding
  override def sourceString = outputBinding.toString

  // TODO: WOM: Can these also be wrapped in a WomOptional if the cwlExpressionType is '[null, File]'? Write a test and see what cromwell/salad produces
  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet): ErrorOr[WomValue] = {
    outputBinding.generateOutputWomValue(
      inputValues,
      ioFunctionSet,
      cwlExpressionType,
      secondaryFilesCoproduct,
      formatCoproduct)
  }

  /**
    * Returns the list of files that _will be_ output after the command is run.
    *
    * In CWL, a list of outputs is specified as glob, say `*.bam`, plus a list of secondary files that may be in the
    * form of paths or specified using carets such as `^.bai`.
    *
    * The coerceTo may be one of four different values:
    * - WomMaybePopulatedFileType
    * - WomArrayType(WomMaybePopulatedFileType)
    * - WomMaybeListedDirectoryType
    * - WomArrayType(WomMaybeListedDirectoryType) (Possible according to the way the spec is written, but not likely?)
    */
  override def evaluateFiles(inputValues: Map[String, WomValue],
                             ioFunctionSet: IoFunctionSet,
                             coerceTo: WomType): ErrorOr[Set[WomFile]] = {
    outputBinding.primaryAndSecondaryFiles(inputValues, ioFunctionSet, coerceTo, secondaryFilesCoproduct).map(_.toSet)
  }
}

final case class WorkflowStepInputExpression(input: WorkflowStepInput, override val cwlExpressionType: WomType, graphInputs: Set[String])(implicit parentName: ParentName) extends CwlWomExpression {

  override def sourceString = input.toString

  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet) = {
    (input.valueFrom, input.source) match {
      case (None, Some(WorkflowStepInputSource.String(id))) =>
        inputValues.
          get(FullyQualifiedName(id).id).
          toValidNel(s"could not find id $id in typeMap\n${inputValues.mkString("\n")}\nwhen evaluating $input.  Graph Inputs were ${graphInputs.mkString("\n")}")
      case _ => s"Could not do evaluateValue(${input.valueFrom}, ${input.source}), most likely it has not been implemented yet".invalidNel
    }
  }

  override def evaluateFiles(inputTypes: Map[String, WomValue], ioFunctionSet: IoFunctionSet, coerceTo: WomType) =
    "Programmer error: Shouldn't use WorkflowStepInputExpressions to find output files. You silly goose.".invalidNel

  override def inputs = graphInputs ++ input.source.toSet.flatMap{ inputSource: InputSource => inputSource match {
    case WorkflowStepInputSource.String(s) => Set(FullyQualifiedName(s).id)
    case WorkflowStepInputSource.StringArray(sa) => sa.map(FullyQualifiedName(_).id).toSet
  }}
}

final case class InitialWorkDirFileGeneratorExpression(entry: IwdrListingArrayEntry) extends CwlWomExpression {
  override def cwlExpressionType: WomType = WomMaybePopulatedFileType
  override def sourceString: String = entry.toString

  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet): ErrorOr[WomFile] = {
    def mustBeString(womValue: WomValue): ErrorOr[String] = womValue match {
      case WomString(s) => s.validNel
      case other => WomStringType.coerceRawValue(other).map(_.asInstanceOf[WomString].value).toErrorOr
    }

    def evaluateEntryName(stringOrExpression: StringOrExpression): ErrorOr[String] = stringOrExpression match {
      case StringOrExpression.String(s) => s.validNel
      case StringOrExpression.Expression(entrynameExpression) => for {
        entryNameExpressionEvaluated <- ExpressionEvaluator.eval(entrynameExpression, ParameterContext(inputValues))
        entryNameValidated <- mustBeString(entryNameExpressionEvaluated)
      } yield entryNameValidated
    }

    entry match {
      case IwdrListingArrayEntry.StringDirent(content, direntEntryName, _) => for {
        entryNameValidated <- evaluateEntryName(direntEntryName)
        writtenFile <- validate(Await.result(ioFunctionSet.writeFile(entryNameValidated, content), Duration.Inf))
      } yield writtenFile

      case IwdrListingArrayEntry.ExpressionDirent(content, direntEntryName, _) =>
        val entryEvaluation: ErrorOr[WomValue] = ExpressionEvaluator.eval(content, ParameterContext(inputValues))
        entryEvaluation flatMap {
          // TODO CWL: Once files have "local paths", we will be able to specify a new local name based on direntEntryName if necessary.
          case f: WomFile => f.validNel
          case other => for {
            coerced <- WomStringType.coerceRawValue(other).toErrorOr
            contentString = coerced.asInstanceOf[WomString].value
            // We force the entryname to be specified, and then evaluate it:
            entryNameUnoptioned <- direntEntryName.toErrorOr("Invalid dirent: Entry was a string but no file name was supplied")
            entryname <- evaluateEntryName(entryNameUnoptioned)
            writtenFile <- validate(Await.result(ioFunctionSet.writeFile(entryname, contentString), Duration.Inf))
          } yield writtenFile
        }

      case _ => ??? // TODO WOM and the rest....
    }
  }


  override def evaluateFiles(inputTypes: Map[String, WomValue], ioFunctionSet: IoFunctionSet, coerceTo: WomType): ErrorOr[Set[WomFile]] =
    "Programmer error: Shouldn't use InitialWorkDirRequirement listing to find output files. You silly goose.".invalidNel

  /**
    * We already get all of the task inputs when evaluating, and we don't need to highlight anything else
    */
  override def inputs: Set[String] = Set.empty
}
