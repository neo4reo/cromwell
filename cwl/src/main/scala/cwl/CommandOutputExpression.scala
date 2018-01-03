package cwl

import common.validation.ErrorOr.ErrorOr
import wom.expression.IoFunctionSet
import wom.types._
import wom.values.{WomArray, WomFile, WomGlobFile, WomMap, WomString, WomValue}
import cats.syntax.validated._
import cats.syntax.either._

import common.validation.Validation._
import cats.data.NonEmptyList

import scala.language.postfixOps
import scala.concurrent.Await
import scala.concurrent.duration._

case class CommandOutputExpression(outputBinding: CommandOutputBinding,
                                   override val cwlExpressionType: WomType,
                                   override val inputs: Set[String]) extends CwlWomExpression {

  // TODO WOM: outputBinding.toString is probably not the best representation of the expression source
  override def sourceString = outputBinding.toString

  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet): ErrorOr[WomValue] = {

    val parameterContext = ParameterContext().addInputs(inputValues)

    /*
    CommandOutputBinding.glob:
    Find files relative to the output directory, using POSIX glob(3) pathname matching. If an array is provided, find
    files that match any pattern in the array. If an expression is provided, the expression must return a string or an
    array of strings, which will then be evaluated as one or more glob patterns. Must only match and return files which
    actually exist.

    http://www.commonwl.org/v1.0/CommandLineTool.html#CommandOutputBinding
     */
    def outputBindingEvaluationResult: Either[NonEmptyList[String], WomValue] = {
      import StringOrExpression._
      (outputBinding, parameterContext) match {
        case (CommandOutputBinding(_, _, Some(String(value))), Right(_)) => WomString(value).asRight
        case (CommandOutputBinding(Some(glob), _, None), Right(parameterContext)) =>
              WomArray(WomArrayType(WomStringType), GlobEvaluator.globPaths(glob, parameterContext, ioFunctionSet).map(WomString.apply)).asRight

        case (CommandOutputBinding(glob, loadContents, Some(Expression(expression))), Right(parameterContext)) =>

          val paths: Seq[String] = glob.toSeq flatMap { globValue =>
            GlobEvaluator.globPaths(globValue, parameterContext, ioFunctionSet)
          }

          val _loadContents: Boolean = loadContents getOrElse false

          val womMaps: Array[Map[String, String]] =
            paths.toArray map({
              (path:String) =>
                // TODO: WOM: basename/dirname/size/checksum/etc.
                val contents =
                    Map("contents" -> load64KiB(path, ioFunctionSet)).
                      filter(_ => _loadContents)

                val location = Map("location" -> path)

                location ++ contents
            })

          val outputEvalParameterContext: ParameterContext = parameterContext.setSelf(womMaps)

          expression.fold(EvaluateExpression).apply(outputEvalParameterContext).toEither.leftMap(e => NonEmptyList.one(e.getMessage))
      }
    }
    //To facilitate ECMAScript evaluation, filenames are stored in a map under the key "location"
    val womValue =
      outputBindingEvaluationResult map {
        case WomArray(_, Seq(WomMap(WomMapType(WomStringType, WomStringType), results))) => results(WomString("location"))
        case other => other
      }

    //If the value is a string but the output is expecting a file, we consider that string a POSIX "glob" and apply
    //it accordingly to retrieve the file list to which it expands.
    val globbedIfFile:ErrorOr[WomValue] =
      (womValue, cwlExpressionType) match {

        //In the case of a single file being expected, we must enforce that the glob only represents a single file
        case (Right(WomString(glob)), WomSingleFileType) =>
          Await.result(ioFunctionSet.glob(glob), Duration.Inf) match {
            case head :: Nil => WomString(head).validNel
            case list => s"expecting a single File glob but instead got ${list.toList.mkString(", ")}".invalidNel
          }

        case (other, _) => other.toValidated
      }

    //CWL tells us the type this output is expected to be.  Attempt to coerce the actual output into this type.
    globbedIfFile.toTry.flatMap(cwlExpressionType.coerceRawValue).toErrorOr
  }

  /*
  TODO:
   DB: It doesn't make sense to me that this function returns type WomFile but accepts a type to which it coerces.
   Wouldn't coerceTo always == WomFileType, and if not then what?
   */
  override def evaluateFiles(inputs: Map[String, WomValue], ioFunctionSet: IoFunctionSet, coerceTo: WomType): ErrorOr[Set[WomFile]] ={

    ParameterContext().addInputs(inputs).
      map(pc =>
        for {
          globValue <- outputBinding.glob.toList
          path <- GlobEvaluator.globPaths(globValue, pc, ioFunctionSet).toList
        } yield WomGlobFile(path): WomFile).
      map(_.toSet).
      toValidated

  }

  private def load64KiB(path: String, ioFunctionSet: IoFunctionSet): String = {
    // This suggests the IoFunctionSet should have a length-limited read API as both CWL and WDL support this concept.
    // ChrisL: But remember that they are different (WDL => failure, CWL => truncate)
    val content = ioFunctionSet.readFile(path)

    // TODO: propagate IO, Try, or Future or something all the way out via "commandOutputBindingtoWomValue" signature
    // TODO: Stream only the first 64 KiB, this "read everything then ignore most of it" method will be very inefficient
    val initialResult = Await.result(content, 60 seconds)
    initialResult.substring(0, Math.min(initialResult.length, 64 * 1024))
  }
}
