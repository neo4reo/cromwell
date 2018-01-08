package cwl

import java.nio.file.Paths

import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.validated._
import com.typesafe.config.ConfigFactory
import common.Checked
import common.validation.ErrorOr._
import common.validation.Validation._
import cwl.CommandLineTool._
import cwl.CwlType.CwlType
import cwl.CwlVersion._
import cwl.command.ParentName
import cwl.requirement.RequirementToAttributeMap
import eu.timepit.refined.W
import shapeless.syntax.singleton._
import shapeless.{:+:, CNil, Coproduct, Poly1, Witness}
import wom.callable.Callable.{InputDefinitionWithDefault, OutputDefinition, RequiredInputDefinition}
import wom.callable.{Callable, CallableTaskDefinition}
import wom.executable.Executable
import wom.expression.{InputLookupExpression, ValueAsAnExpression, WomExpression}
import wom.types.WomType
import wom.values.{WomArray, WomFile, WomString, WomValue}
import wom.{CommandPart, RuntimeAttributes}

import scala.language.postfixOps
import scala.util.Try

/**
  * @param `class` This _should_ always be "CommandLineTool," however the spec does not -er- specify this.
  */
case class CommandLineTool private(
                                   inputs: Array[CommandInputParameter],
                                   outputs: Array[CommandOutputParameter],
                                   `class`: Witness.`"CommandLineTool"`.T,
                                   id: String,
                                   requirements: Option[Array[Requirement]],
                                   hints: Option[Array[Hint]],
                                   label: Option[String],
                                   doc: Option[String],
                                   cwlVersion: Option[CwlVersion],
                                   baseCommand: Option[BaseCommand],
                                   arguments: Option[Array[CommandLineTool.Argument]],
                                   stdin: Option[StringOrExpression],
                                   stderr: Option[StringOrExpression],
                                   stdout: Option[StringOrExpression],
                                   successCodes: Option[Array[Int]],
                                   temporaryFailCodes: Option[Array[Int]],
                                   permanentFailCodes: Option[Array[Int]]) {

  private [cwl] implicit val explicitWorkflowName = ParentName(id)
  private val inputNames = this.inputs.map(i => FullyQualifiedName(i.id).id).toSet

  /** Builds an `Executable` directly from a `CommandLineTool` CWL with no parent workflow. */
  def womExecutable(validator: RequirementsValidator, inputFile: Option[String] = None): Checked[Executable] = {
    val taskDefinition = buildTaskDefinition(parentWorkflow = None, validator)
    CwlExecutableValidation.buildWomExecutable(taskDefinition, inputFile)
  }

  private def validateRequirementsAndHints(parentWorkflow: Option[Workflow], validator: RequirementsValidator): ErrorOr[List[Requirement]] = {
    import cats.instances.list._
    import cats.syntax.traverse._

    val allRequirements = requirements.toList.flatten ++ parentWorkflow.toList.flatMap(_.allRequirements)
    // All requirements must validate or this fails.
    val errorOrValidatedRequirements: ErrorOr[List[Requirement]] = allRequirements traverse validator

    errorOrValidatedRequirements map { validRequirements =>
      // Only Requirement hints, everything else is thrown out.
      // TODO CWL don't throw them out but pass them back to the caller to do with as the caller pleases.
      val hintRequirements = hints.toList.flatten.flatMap { _.select[Requirement] }
      val parentHintRequirements = parentWorkflow.toList.flatMap(_.allHints)

      // Throw out invalid Requirement hints.
      // TODO CWL pass invalid hints back to the caller to do with as the caller pleases.
      val validHints = (hintRequirements ++ parentHintRequirements).collect { case req if validator(req).isValid => req }
      validRequirements ++ validHints
    }
  }

  private def processRequirement(requirement: Requirement): Map[String, WomExpression] = {
    requirement.fold(RequirementToAttributeMap).apply(inputNames)
  }

  def buildTaskDefinition(parentWorkflow: Option[Workflow], validator: RequirementsValidator): ErrorOr[CallableTaskDefinition] = {
    validateRequirementsAndHints(parentWorkflow, validator) map { requirementsAndHints =>
      val id = this.id

      val commandTemplate: Seq[CommandPart] = baseCommand.toSeq.flatMap(_.fold(BaseCommandToCommandParts)) ++
        arguments.toSeq.flatMap(_.map(_.fold(ArgumentToCommandPart))) ++
        CommandLineTool.orderedForCommandLine(inputs).map(InputParameterCommandPart.apply)

      // This is basically doing a `foldMap` but can't actually be a `foldMap` because:
      // - There is no monoid instance for `WomExpression`s.
      // - We want to fold from the right so the hints and requirements with the lowest precedence are processed first
      //   and later overridden if there are duplicate hints or requirements of the same type with higher precedence.
      val finalAttributesMap = (requirementsAndHints ++ DefaultDockerRequirement).foldRight(Map.empty[String, WomExpression])({
        case (requirement, attributesMap) => attributesMap ++ processRequirement(requirement)
      })

      val runtimeAttributes: RuntimeAttributes = RuntimeAttributes(finalAttributesMap)

      val meta: Map[String, String] = Map.empty
      val parameterMeta: Map[String, String] = Map.empty

      /*
    quoted from: http://www.commonwl.org/v1.0/CommandLineTool.html#CommandOutputBinding :

    For inputs and outputs, we only keep the variable name in the definition
     */

    val outputs: List[Callable.OutputDefinition] = this.outputs.map {
      case CommandOutputParameter(outputId, _, secondaryFiles, format, _, _, Some(outputBinding), Some(tpe)) =>
        val womType = tpe.fold(MyriadOutputTypeToWomType)
        val outputExpression = CommandOutputExpression(outputBinding, womType, inputNames, secondaryFiles, format)
        OutputDefinition(FullyQualifiedName(outputId).id, womType, outputExpression)

      //This catches states where the output binding is not declared but the type is
      case CommandOutputParameter(outputId, _, _, _, _, _, _, Some(tpe)) =>
        val womType: WomType = tpe.fold(MyriadOutputTypeToWomType)
        OutputDefinition(FullyQualifiedName(outputId).id, womType, InputLookupExpression(womType, outputId))

      case other => throw new NotImplementedError(s"Command output parameters such as $other are not yet supported")
    }.toList

    val inputDefinitions: List[_ <: Callable.InputDefinition] =
      this.inputs.map {
        case CommandInputParameter(inputId, _, _, _, _, _, _, Some(default), Some(tpe)) =>
          val inputType = tpe.fold(MyriadInputTypeToWomType)
          val inputName = FullyQualifiedName(inputId).id
          val defaultWomValue = default.fold(CommandInputParameter.DefaultToWomValuePoly).apply(inputType).toTry.get
          InputDefinitionWithDefault(inputName, inputType, ValueAsAnExpression(defaultWomValue))
        case CommandInputParameter(inputId, _, _, _, _, _, _, None, Some(tpe)) =>
          val inputType = tpe.fold(MyriadInputTypeToWomType)
          val inputName = FullyQualifiedName(inputId).id
          RequiredInputDefinition(inputName, inputType)
        case other => throw new NotImplementedError(s"command input parameters such as $other are not yet supported")
      }.toList

      def stringOrExpressionToString(soe: Option[StringOrExpression]): Option[String] = soe flatMap {
        case StringOrExpression.String(str) => Some(str)
        case StringOrExpression.Expression(_) => None // ... for now!
      }

      // The try will succeed if this is a task within a step. If it's a standalone file, the ID will be the file,
      // so the filename is the fallback.
      def taskName = Try(FullyQualifiedName(id).id).getOrElse(Paths.get(id).getFileName.toString)

      val adHocFileCreations: Set[WomExpression] = (for {
        requirements <- requirements.getOrElse(Array.empty[Requirement])
        initialWorkDirRequirement <- requirements.select[InitialWorkDirRequirement].toArray
        listing <- initialWorkDirRequirement.listings
      } yield InitialWorkDirFileGeneratorExpression(listing)).toSet[WomExpression]

      CallableTaskDefinition(
        taskName,
        commandTemplate,
        runtimeAttributes,
        meta,
        parameterMeta,
        outputs,
        inputDefinitions,
        // TODO: This doesn't work in all cases and it feels clunky anyway - find a way to sort that out
        prefixSeparator = "#",
        commandPartSeparator = " ",
        stdoutRedirection = stringOrExpressionToString(stdout),
        stderrRedirection = stringOrExpressionToString(stderr),
        adHocFileCreation = adHocFileCreations
      )
    }
  }

  def asCwl = Coproduct[Cwl](this)

}

object CommandLineTool {

  /**
    * Sort according to position. If position does not exist, use 0 per spec:
    * http://www.commonwl.org/v1.0/CommandLineTool.html#CommandLineBinding
    *
    * If an input binding is not specified, ignore the input parameter.
    */
  protected[cwl] def orderedForCommandLine(inputs: Array[CommandInputParameter]): Seq[CommandInputParameter] = {
    inputs.
      filter(_.inputBinding.isDefined).
      sortBy(_.inputBinding.flatMap(_.position).getOrElse(0)).
      toSeq
  }

  def apply(inputs: Array[CommandInputParameter] = Array.empty,
            outputs: Array[CommandOutputParameter] = Array.empty,
            id: String,
            requirements: Option[Array[Requirement]] = None,
            hints: Option[Array[Hint]] = None,
            label: Option[String] = None,
            doc: Option[String] = None,
            cwlVersion: Option[CwlVersion] = Option(CwlVersion.Version1),
            baseCommand: Option[BaseCommand] = None,
            arguments: Option[Array[CommandLineTool.Argument]] = None,
            stdin: Option[StringOrExpression] = None,
            stderr: Option[StringOrExpression] = None,
            stdout: Option[StringOrExpression] = None,
            successCodes: Option[Array[Int]] = None,
            temporaryFailCodes: Option[Array[Int]] = None,
            permanentFailCodes: Option[Array[Int]] = None): CommandLineTool =
    CommandLineTool(inputs, outputs, "CommandLineTool".narrow, id, requirements, hints, label, doc, cwlVersion, baseCommand, arguments, stdin, stderr, stdout, successCodes, temporaryFailCodes, permanentFailCodes)

  type BaseCommand = SingleOrArrayOfStrings

  type Argument = CommandLineBinding :+: StringOrExpression :+: CNil

  case class CommandInputParameter(
                                    id: String,
                                    label: Option[String] = None,
                                    secondaryFiles: Option[Array[StringOrExpression]] = None,
                                    format: Option[Expression :+: Array[String] :+: String :+: CNil] = None, //only valid when type: File
                                    streamable: Option[Boolean] = None, //only valid when type: File
                                    doc: Option[String :+: Array[String] :+: CNil] = None,
                                    inputBinding: Option[CommandLineBinding] = None,
                                    default: Option[CwlAny] = None,
                                    `type`: Option[MyriadInputType] = None)

  object CommandInputParameter {

    type DefaultToWomValueFunction = WomType => ErrorOr[WomValue]

    object DefaultToWomValuePoly extends Poly1 {
      implicit def caseFileOrDirectory: Case.Aux[FileOrDirectory, DefaultToWomValueFunction] = {
        at {
          _.fold(this)
        }
      }

      implicit def caseFileOrDirectoryArray: Case.Aux[Array[FileOrDirectory], DefaultToWomValueFunction] = {
        at {
          fileOrDirectoryArray =>
            womType =>
              fileOrDirectoryArray
                .toList
                .traverse[ErrorOr, WomValue](_.fold(this).apply(womType))
                .map(WomArray(_))
        }
      }

      implicit def caseFile: Case.Aux[File, DefaultToWomValueFunction] = {
        at {
          file =>
            womType =>
              file.asWomValue.flatMap(womType.coerceRawValue(_).toErrorOr)
        }
      }

      implicit def caseDirectory: Case.Aux[Directory, DefaultToWomValueFunction] = {
        at {
          directory =>
            womType =>
              directory.asWomValue.flatMap(womType.coerceRawValue(_).toErrorOr)
        }
      }

      implicit def caseJson: Case.Aux[io.circe.Json, DefaultToWomValueFunction] = {
        at {
          circeJson =>
            womType =>
              val stringJson = circeJson.noSpaces
              import spray.json._
              val sprayJson = stringJson.parseJson
              womType.coerceRawValue(sprayJson).toErrorOr
        }
      }
    }
  }

  case class CommandInputRecordSchema(
                                       `type`: W.`"record"`.T,
                                       fields: Option[Array[CommandInputRecordField]],
                                       label: Option[String])

  case class CommandInputRecordField(
                                      name: String,
                                      `type`: MyriadInputType,
                                      doc: Option[String],
                                      inputBinding: Option[CommandLineBinding],
                                      label: Option[String])

  case class CommandInputEnumSchema(
                                     symbols: Array[String],
                                     `type`: W.`"enum"`.T,
                                     label: Option[String],
                                     inputBinding: Option[CommandLineBinding])

  case class CommandInputArraySchema(
                                      items:
                                      CwlType :+:
                                        CommandInputRecordSchema :+:
                                        CommandInputEnumSchema :+:
                                        CommandInputArraySchema :+:
                                        String :+:
                                        Array[
                                          CwlType :+:
                                            CommandInputRecordSchema :+:
                                            CommandInputEnumSchema :+:
                                            CommandInputArraySchema :+:
                                            String :+:
                                            CNil] :+:
                                        CNil,
                                      `type`: W.`"array"`.T,
                                      label: Option[String],
                                      inputBinding: Option[CommandLineBinding])


  case class CommandOutputParameter(
                                     id: String,
                                     label: Option[String] = None,
                                     secondaryFiles: Option[SecondaryFiles] = None,
                                     format: Option[StringOrExpression] = None, //only valid when type: File
                                     streamable: Option[Boolean] = None, //only valid when type: File
                                     doc: Option[String :+: Array[String] :+: CNil] = None,
                                     outputBinding: Option[CommandOutputBinding] = None,
                                     `type`: Option[MyriadOutputType] = None)

  object CommandOutputParameter {

    type FormatFunction = ParameterContext => ErrorOr[String]

    object FormatPoly extends Poly1 {
      implicit def caseStringOrExpression: Case.Aux[StringOrExpression, FormatFunction] = {
        at {
          _.fold(this)
        }
      }

      implicit def caseExpression: Case.Aux[Expression, FormatFunction] = {
        at {
          expression =>
            parameterContext =>
              val result: ErrorOr[WomValue] = ExpressionEvaluator.eval(expression, parameterContext)
              result flatMap {
                case womString: WomString => womString.value.valid
                case other => s"Not a valid file format: $other".invalidNel
              }
        }
      }

      implicit def caseString: Case.Aux[String, FormatFunction] = at { string => _ => string.valid }
    }

    type SecondaryFilesFunction = (WomFile, ParameterContext) => ErrorOr[List[WomFile]]

    object SecondaryFilesPoly extends Poly1 {
      implicit def caseStringOrExpression: Case.Aux[StringOrExpression, SecondaryFilesFunction] = {
        at {
          _.fold(this)
        }
      }

      implicit def caseExpression: Case.Aux[Expression, SecondaryFilesFunction] = {
        at {
          expression =>
            (primaryWomFile, parameterContext) =>
              File.secondaryExpressionFiles(primaryWomFile, expression, parameterContext)
        }
      }

      implicit def caseString: Case.Aux[String, SecondaryFilesFunction] = {
        at {
          string =>
            (primaryWomFile, _) =>
              File.secondaryStringFile(primaryWomFile, string).map(List(_))
        }
      }

      implicit def caseArray: Case.Aux[Array[StringOrExpression], SecondaryFilesFunction] = {
        at {
          array =>
            (primaryWomFile, parameterContext) =>
              val functions: List[SecondaryFilesFunction] = array.toList.map(_.fold(this))
              functions.flatTraverse(_ (primaryWomFile, parameterContext))
        }
      }
    }

  }

  // Used to supply a default Docker image for platforms like PAPI that must have one even if the CWL document does
  // not specify a `DockerRequirement`.
  import net.ceedubs.ficus.Ficus._
  lazy val DefaultDockerImage = ConfigFactory.load().as[Option[String]]("cwl.default-docker-image")

  lazy val DefaultDockerRequirement = DefaultDockerImage map { image => Coproduct[Requirement](DockerRequirement(
    `class` = "DockerRequirement",
    dockerPull = Option(image),
    dockerLoad = None,
    dockerFile = None,
    dockerImport = None,
    dockerImageId = None,
    dockerOutputDirectory = None
  )) } toList

}
