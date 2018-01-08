package cwl

import cats.instances.list._
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.traverse._
import cats.syntax.validated._
import common.validation.ErrorOr._
import common.validation.Validation._
import wom.expression.IoFunctionSet
import wom.types._
import wom.values._

import scala.concurrent.Await
import scala.concurrent.duration._

/** @see <a href="http://www.commonwl.org/v1.0/Workflow.html#CommandOutputBinding">CommandOutputBinding</a> */
case class CommandOutputBinding(
                                 glob: Option[Glob] = None,
                                 loadContents: Option[Boolean] = None,
                                 outputEval: Option[StringOrExpression] = None) {

  /**
    * Returns the list of globs for this command output binding.
    */
  def primaryCwlGlobPaths(parameterContext: ParameterContext): ErrorOr[List[String]] = {
    glob map {
      _.fold(GlobEvaluator.GlobEvaluatorPoly).apply(parameterContext)
    } getOrElse {
      Nil.valid
    }
  }

  /**
    * Returns the list of secondary files for the primary file.
    */
  def secondariesFromPrimaryFile(parameterContext: ParameterContext,
                                 primaryWomFile: WomFile,
                                 secondaryFilesOption: Option[SecondaryFiles]): ErrorOr[List[WomFile]] = {
    secondaryFilesOption.map(
      _
        .fold(CommandLineTool.CommandOutputParameter.SecondaryFilesPoly)
        .apply(primaryWomFile, parameterContext)
    ).getOrElse(Nil.valid)
  }

  /**
    * Returns all the primary and secondary files that _will be_ created by this command output binding.
    */
  def primaryAndSecondaryFiles(inputValues: Map[String, WomValue],
                               ioFunctionSet: IoFunctionSet,
                               outputWomType: WomType,
                               secondaryFilesOption: Option[SecondaryFiles]): ErrorOr[List[WomFile]] = {
    val parameterContext = ParameterContext(inputs = inputValues)
    val outputWomFlatType = outputWomType match {
      case WomMaybeListedDirectoryType => WomMaybeListedDirectoryType
      case WomArrayType(WomMaybeListedDirectoryType) => WomMaybeListedDirectoryType
      case _ => WomGlobFileType
    }

    for {
      primaryPaths <- primaryCwlGlobPaths(parameterContext)
      primary <- outputWomFlatType match {
        case WomGlobFileType => primaryPaths.map(WomGlobFile).valid
        case WomMaybeListedDirectoryType => primaryPaths.map(WomMaybeListedDirectory(_)).valid
        case other => s"Program error: $other type was not expected".invalidNel
      }
      secondary <- primary.flatTraverse(secondariesFromPrimaryFile(parameterContext, _, secondaryFilesOption))
    } yield primary ++ secondary
  }

  /**
    * Loads a directory with the files listed, each file with contents populated.
    */
  def loadDirectoryWithListing(path: String, ioFunctionSet: IoFunctionSet): ErrorOr[WomMaybeListedDirectory] = {
    val listing = Await.result(ioFunctionSet.listAllFilesUnderDirectory(path), Duration.Inf)
    listing.toList traverse loadFileWithContents(ioFunctionSet) map { listing =>
      WomMaybeListedDirectory(valueOption = Option(path), listingOption = Option(listing))
    }
  }

  /**
    * Loads a file at path reading 64KiB of data into the contents.
    */
  def loadFileWithContents(ioFunctionSet: IoFunctionSet)(path: String): ErrorOr[WomFile] = {
    val contentsOptionErrorOr = {
      if (loadContents getOrElse false) {
        load64KiB(path, ioFunctionSet).map(Option(_))
      } else {
        None.valid
      }
    }
    contentsOptionErrorOr map { contentsOption =>
      WomMaybePopulatedFile(valueOption = Option(path), contentsOption = contentsOption)
    }
  }

  /**
    * Given a cwl glob path and an output type, gets the listing using the ioFunctionSet, and optionally loads the
    * contents of the file(s).
    */
  def getFilesWithContents(ioFunctionSet: IoFunctionSet,
                           outputWomType: WomType)(cwlPath: String): ErrorOr[List[WomFile]] = {
    /*
    For each file matched in glob, read up to the first 64 KiB of text from the file and place it in the contents field
    of the file object for manipulation by outputEval.
     */
    val womMaybeListedDirectoryOrFileType = outputWomType match {
      case WomMaybeListedDirectoryType => WomMaybeListedDirectoryType
      case WomArrayType(WomMaybeListedDirectoryType) => WomMaybeListedDirectoryType
      case _ => WomMaybePopulatedFileType
    }
    womMaybeListedDirectoryOrFileType match {
      case WomMaybeListedDirectoryType =>
        // Even if multiple directories are _somehow_ requested, a single flattened directory is returned.
        loadDirectoryWithListing(cwlPath, ioFunctionSet).map(List(_))
      case WomMaybePopulatedFileType =>
        val globs = Await.result(ioFunctionSet.glob(cwlPath), Duration.Inf)
        globs.toList traverse loadFileWithContents(ioFunctionSet)
      case other => s"Program error: $other type was not expected".invalidNel
    }
  }

  /**
    * Creates a wom array with the appropriate concrete member type based on the list of directory or files.
    *
    * If the array is empty, creates an array of WomMaybePopulatedFile.
    */
  def toWomFilesArray(womDirectoryOrFiles: List[WomFile]): WomArray = {
    val womArrayType = WomArrayType(womDirectoryOrFiles.headOption.map(_.womType).getOrElse(WomMaybePopulatedFileType))
    WomArray(womArrayType, womDirectoryOrFiles)
  }

  /**
    * Generates an output wom value based on the specification in command output binding.
    *
    * Depending on the outputWomType, the following steps will be applied as specified in the CWL spec:
    * 1. glob: get a list the globbed files as our primary files
    * 2. loadContents: load the contents of the primary files
    * 3. outputEval: pass in the primary files to an expression to generate our return value
    * 4. secondaryFiles: just before returning the value, fill in the secondary files on the return value
    *
    * The result type will be coerced to the output type.
    */
  def generateOutputWomValue(inputValues: Map[String, WomValue],
                             ioFunctionSet: IoFunctionSet,
                             outputWomType: WomType,
                             secondaryFilesCoproduct: Option[SecondaryFiles],
                             formatCoproduct: Option[StringOrExpression]): ErrorOr[WomValue] = {
    val parameterContext = ParameterContext(inputs = inputValues)

    // 3. outputEval: pass in the primary files to an expression to generate our return value
    def evaluateWomValue(womFilesArray: WomArray): ErrorOr[WomValue] = {
      outputEval match {
        case Some(StringOrExpression.String(string)) => WomString(string).valid
        case Some(StringOrExpression.Expression(expression)) =>
          val outputEvalParameterContext = parameterContext.copy(self = womFilesArray)
          ExpressionEvaluator.eval(expression, outputEvalParameterContext)
        case None =>
          womFilesArray.valid
      }
    }

    // Used to retrieve the file format to be injected into a file result.
    def formatOptionErrorOr = formatCoproduct.traverse[ErrorOr, String](
      _.fold(CommandLineTool.CommandOutputParameter.FormatPoly).apply(parameterContext)
    )

    // 4. secondaryFiles: just before returning the value, fill in the secondary files on the return value
    def populateSecondaryFiles(evaluatedWomValue: WomValue): ErrorOr[WomValue] = {
      evaluatedWomValue match {
        case womMaybePopulatedFile: WomMaybePopulatedFile =>
          val secondaryFilesErrorOr =
            secondariesFromPrimaryFile(parameterContext, womMaybePopulatedFile, secondaryFilesCoproduct)

          (secondaryFilesErrorOr, formatOptionErrorOr) mapN { (secondaryFiles, formatOption) =>
            womMaybePopulatedFile.copy(secondaryFiles = secondaryFiles, formatOption = formatOption)
          }

        case womArray: WomArray if womArray.womType.memberType == WomMaybePopulatedFileType =>
          formatOptionErrorOr map { formatOption =>
            womArray.map(_.asInstanceOf[WomMaybePopulatedFile].copy(formatOption = formatOption))
          }

        case womValue: WomValue => womValue.valid
      }
    }

    // CWL tells us the type this output is expected to be. Attempt to coerce the actual output into this type.
    def coerceWomValue(populatedWomValue: WomValue): ErrorOr[WomValue] = {
      (outputWomType, populatedWomValue) match {
        case (womType: WomArrayType, womValue: WomArray) => womType.coerceRawValue(womValue).toErrorOr
        case (womType: WomArrayType, womValue) =>
          // Coerce a single value to an array
          womType.coerceRawValue(womValue).toErrorOr.map(value => WomArray(womType, List(value)))
        case (womType, womValue: WomArray) if womValue.value.lengthCompare(1) == 0 =>
          // Coerce an array to a single value
          womType.coerceRawValue(womValue.value.head).toErrorOr
        case (womType, womValue) => womType.coerceRawValue(womValue).toErrorOr
      }
    }

    for {
      // 1. glob: get a list the globbed files as our primary files
      primaryPaths <- primaryCwlGlobPaths(parameterContext)

      // 2. loadContents: load the contents of the primary files
      primaryAsDirectoryOrFiles <- primaryPaths.flatTraverse(getFilesWithContents(ioFunctionSet, outputWomType))
      womFilesArray = toWomFilesArray(primaryAsDirectoryOrFiles)

      // 3. outputEval: pass in the primary files to an expression to generate our return value
      evaluatedWomValue <- evaluateWomValue(womFilesArray)

      // 4. secondaryFiles: just before returning the value, fill in the secondary files on the return value
      populatedWomValue <- populateSecondaryFiles(evaluatedWomValue)

      // CWL tells us the type this output is expected to be. Attempt to coerce the actual output into this type.
      coercedWomValue <- coerceWomValue(populatedWomValue)
    } yield coercedWomValue
  }

  private def load64KiB(path: String, ioFunctionSet: IoFunctionSet): ErrorOr[String] = {
    // This suggests the IoFunctionSet should have a length-limited read API as both CWL and WDL support this concept.
    // ChrisL: But remember that they are different (WDL => failure, CWL => truncate)
    val content = ioFunctionSet.readFile(path)

    // TODO: WOM: propagate Future (or IO?) signature
    // TODO: WOM: Stream only the first 64 KiB, this "read everything then ignore most of it" method is terrible
    validate {
      val initialResult = Await.result(content, 5.seconds)
      initialResult.substring(0, Math.min(initialResult.length, 64 * 1024))
    }
  }
}
