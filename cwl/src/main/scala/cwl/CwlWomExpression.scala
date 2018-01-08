package cwl

import cats.syntax.option._
import common.validation.ErrorOr.ErrorOr
import common.validation.Validation._
import common.validation.ErrorOr.ShortCircuitingFlatMap
import cats.syntax.validated._
import cats.syntax.traverse._
import cats.instances.list._
import cwl.InitialWorkDirRequirement.IwdrListingArrayEntry
import cwl.WorkflowStepInput.InputSource
import cwl.command.ParentName
import wom.expression.{IoFunctionSet, WomExpression}
import wom.types._
import wom.values._

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Try

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
    val pc = ParameterContext().withInputs(inputValues, ioFunctionSet)
    expression.fold(EvaluateExpression).apply(pc).toErrorOr
  }

  override def evaluateFiles(inputTypes: Map[String, WomValue], ioFunctionSet: IoFunctionSet, coerceTo: WomType) = Set.empty[WomFile].validNel
}

case class CommandOutputExpression(outputBinding: CommandOutputBinding,
                                   override val cwlExpressionType: WomType,
                                   override val inputs: Set[String]) extends CwlWomExpression {

  // TODO WOM: outputBinding.toString is probably not be the best representation of the outputBinding
  override def sourceString = outputBinding.toString

  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet): ErrorOr[WomValue] = {
    val parameterContext = ParameterContext.Empty.withInputs(inputValues, ioFunctionSet)

    //To facilitate ECMAScript evaluation, filenames are stored in a map under the key "location"
    val womValue = outputBinding.
      commandOutputBindingToWomValue(parameterContext, ioFunctionSet) match {
        case WomArray(_, Seq(WomMap(WomMapType(WomStringType, WomStringType), map))) => map(WomString("location"))
        case other => other
      }

    //If the value is a string but the output is expecting a file, we consider that string a POSIX "glob" and apply
    //it accordingly to retrieve the file list to which it expands.
    val globbedIfFile =
      (womValue, cwlExpressionType) match {

        //In the case of a single file being expected, we must enforce that the glob only represents a single file
        case (WomString(glob), WomSingleFileType) =>
          Await.result(ioFunctionSet.glob(glob), Duration.Inf) match {
            case head :: Nil => WomString(head)
            case list => throw new RuntimeException(s"expecting a single File glob but instead got $list")
          }

        case _ => womValue
      }

    //CWL tells us the type this output is expected to be.  Attempt to coerce the actual output into this type.
    cwlExpressionType.coerceRawValue(globbedIfFile).toErrorOr
  }

  /*
  TODO:
   DB: It doesn't make sense to me that this function returns type WomFile but accepts a type to which it coerces.
   Wouldn't coerceTo always == WomFileType, and if not then what?
   */
  override def evaluateFiles(inputs: Map[String, WomValue], ioFunctionSet: IoFunctionSet, coerceTo: WomType): ErrorOr[Set[WomFile]] = {

    val pc = ParameterContext().withInputs(inputs, ioFunctionSet)

    val files = for {
      globValue <- outputBinding.glob.toList
      path <- GlobEvaluator.globPaths(globValue, pc, ioFunctionSet).toList
    } yield WomGlobFile(path): WomFile

    files.toSet.validNel[String]
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
  override def cwlExpressionType: WomType = WomSingleFileType
  override def sourceString: String = entry.toString

  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet): ErrorOr[WomValue] = {
    def mustBeString(womValue: WomValue): ErrorOr[String] = womValue match {
      case WomString(s) => s.validNel
      case other => WomStringType.coerceRawValue(other).map(_.asInstanceOf[WomString].value).toErrorOr
    }

    def evaluateEntryName(stringOrExpression: StringOrExpression): ErrorOr[String] = stringOrExpression match {
      case StringOrExpression.String(s) => s.validNel
      case StringOrExpression.ECMAScriptExpression(entrynameExpression) => for {
        entryNameExpressionEvaluated <- ExpressionEvaluator.evalExpression(entrynameExpression, ParameterContext().withInputs(inputValues, ioFunctionSet)).toErrorOr
        entryNameValidated <- mustBeString(entryNameExpressionEvaluated)
      } yield entryNameValidated
    }

    entry match {
      case IwdrListingArrayEntry.StringDirent(content, direntEntryName, _) => for {
        entryNameValidated <- evaluateEntryName(direntEntryName)
        writtenFile <- Try(Await.result(ioFunctionSet.writeFile(entryNameValidated, content), Duration.Inf)).toErrorOr
      } yield writtenFile

      case IwdrListingArrayEntry.ExpressionDirent(Expression.ECMAScriptExpression(contentExpression), direntEntryName, _) =>
        val entryEvaluation: ErrorOr[WomValue] = ExpressionEvaluator.evalExpression(contentExpression, ParameterContext().withInputs(inputValues, ioFunctionSet)).toErrorOr
        entryEvaluation flatMap {
          case f: WomFile =>
            val entryName: ErrorOr[String] = direntEntryName match {
              case Some(en) => evaluateEntryName(en)
              case None => f.value.split('/').last.validNel
            }
            entryName flatMap { en => Try(Await.result(ioFunctionSet.copyFile(f.value, en), Duration.Inf)).toErrorOr }
          case other => for {
            coerced <- WomStringType.coerceRawValue(other).toErrorOr
            contentString = coerced.asInstanceOf[WomString].value
            // We force the entryname to be specified, and then evaluate it:
            entryNameUnoptioned <- direntEntryName.toErrorOr("Invalid dirent: Entry was a string but no file name was supplied")
            entryname <- evaluateEntryName(entryNameUnoptioned)
            writtenFile <- Try(Await.result(ioFunctionSet.writeFile(entryname, contentString), Duration.Inf)).toErrorOr
          }  yield writtenFile
        }
      case IwdrListingArrayEntry.ECMAScriptExpression(expression) =>
        // A single expression which must evaluate to an array of Files
        val expressionEvaluation: ErrorOr[WomValue] = ExpressionEvaluator.evalExpression(expression, ParameterContext().withInputs(inputValues, ioFunctionSet)).toErrorOr

        expressionEvaluation flatMap {
          case array: WomArray if WomArrayType(WomSingleFileType).coercionDefined(array) =>
            val newFileArray: ErrorOr[List[WomFile]] = array.value.map(_.valueString).toList.traverse { source: String =>
              val dest = source.split('/').last
              Try(Await.result(ioFunctionSet.copyFile(source, dest), Duration.Inf)).toErrorOr
            }
            newFileArray map { nfa => WomArray(WomArrayType(WomSingleFileType), nfa) }

          case other => s"InitialWorkDirRequirement listing expression must be Array[File] but got ${other.womType.toDisplayString}".invalidNel
        }

      case _ => ??? // TODO CWL and the rest....
    }
  }


  override def evaluateFiles(inputTypes: Map[String, WomValue], ioFunctionSet: IoFunctionSet, coerceTo: WomType): ErrorOr[Set[WomFile]] =
    "Programmer error: Shouldn't use InitialWorkDirRequirement listing to find output files. You silly goose.".invalidNel

  /**
    * We already get all of the task inputs when evaluating, and we don't need to highlight anything else
    */
  override def inputs: Set[String] = Set.empty
}
