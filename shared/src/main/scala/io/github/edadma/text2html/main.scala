package io.github.edadma.text2html

import pprint.*

import scala.language.postfixOps
import java.nio.file.Paths

case class Config(root: String, output: Option[String], scala: Option[String], verbose: Boolean)

@main def run(args: String*): Unit =
  import scopt.OParser
  val builder = OParser.builder[Config]
  val parser = {
    import builder._
    OParser.sequence(
      programName("text2html"),
      head("text2html", "0.0.1"),
      opt[String]('o', "output")
        .valueName("<output path>")
        .action((x, c) => c.copy(output = Some(x)))
        .text("output path"),
      opt[String]('s', "scala")
        .valueName("<package>")
        .action((x, c) => c.copy(scala = Some(x)))
        .text("generate Scala code for <package>"),
      opt[Unit]('v', "verbose")
        .action((_, c) => c.copy(verbose = true))
        .text("verbose console output"),
      help("help").text("prints this usage text"),
      arg[String]("<root path>")
        .action((x, c) => c.copy(root = x))
        .text("root path"),
      checkConfig(c =>
        if c.output.isEmpty && c.root != null then
          val path = Paths get c.root

          if path.getNameCount < 2 then failure("output path needs to be specified") else success
        else success,
      ),
    )
  }

  OParser.parse(parser, args, Config(null, None, None, false)) match {
    case Some(c @ Config(root, None, _, _)) =>
      val path = Paths get root

      App(c.copy(output = Some(path.getParent resolve "text" toString)))
    case Some(c) => App(c)
    case _       =>
  }
