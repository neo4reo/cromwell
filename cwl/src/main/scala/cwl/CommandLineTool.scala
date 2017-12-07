package cwl

import java.nio.file.Paths

import common.Checked
import cwl.CommandLineTool._
import cwl.CwlType.CwlType
import cwl.CwlVersion._
import eu.timepit.refined.W
import shapeless.syntax.singleton._
import shapeless.{:+:, CNil, Coproduct, Witness}
import wom.callable.Callable.{InputDefinitionWithDefault, OutputDefinition, RequiredInputDefinition}
import wom.callable.{Callable, CallableTaskDefinition, ExecutableTaskDefinition}
import wom.executable.Executable
import wom.expression.{LookupExpression, ValueAsAnExpression, WomExpression}
import wom.types.{WomFileType, WomType}
import wom.{CommandPart, RuntimeAttributes}

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
                                   hints: Option[Array[CwlAny]],
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

  def womExecutable(inputFile: Option[String] = None): Checked[Executable] =
    CwlExecutableValidation.buildWomExecutable(ExecutableTaskDefinition.tryApply(taskDefinition).toEither, inputFile)

  def taskDefinition: CallableTaskDefinition = {

    val id = this.id

    val commandTemplate: Seq[CommandPart] = baseCommand.toSeq.flatMap(_.fold(BaseCommandToCommandParts)) ++
      arguments.toSeq.flatMap(_.map(_.fold(ArgumentToCommandPart))) ++
      CommandLineTool.orderedForCommandLine(inputs).map(InputParameterCommandPart.apply)

    val runtimeAttributes: RuntimeAttributes = RuntimeAttributes(Map.empty[String, WomExpression])

    val meta: Map[String, String] = Map.empty
    val parameterMeta: Map[String, String] = Map.empty

    /*
    quoted from: http://www.commonwl.org/v1.0/CommandLineTool.html#CommandOutputBinding :

    For inputs and outputs, we only keep the variable name in the definition

    The output parameter value is generated by applying these operations in the following order:

      glob
      loadContents
      outputEval
      secondaryFiles
     */

    val inputNames = this.inputs.map(i => FullyQualifiedName(i.id).id).toSet

    val outputs: List[Callable.OutputDefinition] = this.outputs.map {
      case CommandOutputParameter(cop_id, _, _, _, _, _, Some(outputBinding), Some(outputType)) if outputType.select[MyriadOutputInnerType].flatMap(_.select[CwlType]).contains(CwlType.File) =>
        OutputDefinition(FullyQualifiedName(cop_id).id, WomFileType, CommandOutputExpression(outputBinding, WomFileType, inputNames))
      case CommandOutputParameter(cop_id, _, _, _, _, _, Some(outputBinding), Some(tpe)) =>
        val womType = tpe.fold(MyriadOutputTypeToWomType)
        OutputDefinition(FullyQualifiedName(cop_id).id, womType, CommandOutputExpression(outputBinding, womType, inputNames))

      case cop:CommandOutputParameter if cop.`type`.isDefined =>
        val womType:WomType = cop.`type`.get.fold(MyriadOutputTypeToWomType)
        OutputDefinition(FullyQualifiedName(cop.id).id, womType, LookupExpression(womType, id))
    }.toList

    val inputDefinitions: List[_ <: Callable.InputDefinition] =
      this.inputs.map { cip =>
        val inputType = cip.`type`.get.fold(MyriadInputTypeToWomType)
        val inputName = FullyQualifiedName(cip.id).id
        cip.default match {
          case Some(d) => InputDefinitionWithDefault(inputName, inputType, ValueAsAnExpression(inputType.coerceRawValue(d.toString).get))
          case None => RequiredInputDefinition(inputName, inputType)
        }
      }.toList

    def stringOrExpressionToString(soe: Option[StringOrExpression]): Option[String] = soe flatMap {
      case StringOrExpression.String(str) => Some(str)
      case StringOrExpression.Expression(_) => None // ... for now!
    }

    // The try will succeed if this is a task within a step. If it's a standalone file, the ID will be the file,
    // so the filename is the fallback.
    def taskName = Try(FullyQualifiedName(id).id).getOrElse(Paths.get(id).getFileName.toString)

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
      stderrRedirection = stringOrExpressionToString(stderr)
    )
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
  protected[cwl] def orderedForCommandLine(inputs: Array[CommandInputParameter]):Seq[CommandInputParameter] = {
    inputs.
      filter(_.inputBinding.isDefined).
      sortBy(_.inputBinding.flatMap(_.position).getOrElse(0)).
      toSeq
  }

  def apply(inputs: Array[CommandInputParameter] = Array.empty,
            outputs: Array[CommandOutputParameter] = Array.empty,
            id: String,
            requirements: Option[Array[Requirement]] = None,
            hints: Option[Array[CwlAny]] = None,
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
            permanentFailCodes: Option[Array[Int]] = None): CommandLineTool  =
              CommandLineTool(inputs, outputs, "CommandLineTool".narrow, id, requirements, hints, label, doc, cwlVersion, baseCommand, arguments, stdin, stderr, stdout, successCodes, temporaryFailCodes, permanentFailCodes)

  type BaseCommand = String :+: Array[String] :+: CNil

  type Argument = Expression :+: CommandLineBinding :+: String :+: CNil

  case class CommandInputParameter(
                                    id: String,
                                    label: Option[String] = None,
                                    secondaryFiles: Option[Array[Expression :+: String :+: CNil]] = None,
                                    format: Option[Expression :+: Array[String] :+: String :+: CNil] = None, //only valid when type: File
                                    streamable: Option[Boolean] = None, //only valid when type: File
                                    doc: Option[String :+: Array[String] :+: CNil] = None,
                                    inputBinding: Option[CommandLineBinding] = None,
                                    default: Option[CwlAny] = None,
                                    `type`: Option[MyriadInputType] = None)

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
                                     secondaryFiles: Option[Expression :+: String :+: Array[Expression :+: String :+: CNil] :+: CNil] = None,
                                     format: Option[Expression :+: Array[String] :+: String :+: CNil] = None, //only valid when type: File
                                     streamable: Option[Boolean] = None, //only valid when type: File
                                     doc: Option[String :+: Array[String] :+: CNil] = None,
                                     outputBinding: Option[CommandOutputBinding] = None,
                                     `type`: Option[MyriadOutputType] = None)



}
