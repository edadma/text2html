package io.github.edadma.text2html

import scala.language.postfixOps
import java.nio.file.{Files, Paths}
import java.io.PrintWriter
import pprint.*

def App(config: Config): Unit =
  val root = Paths get config.root toAbsolutePath
  val output = Paths get config.output.get toAbsolutePath

  def message(m: String): Unit = if config.verbose then println(m)

  message(s"Root directory $root")

  if !Files.exists(root) then problem(s"Directory $root doesn't exist")
  if !Files.isReadable(root) then problem(s"Directory $root is unreadable")

  list(root).filter(p => isDir(p) && !p.getFileName.toString.startsWith(".")) foreach { d =>
    message(s"Processing directory $d")

    val folder = d.getFileName.toString
    val bookName = folder split '-' map (_.capitalize) mkString " "
    val book =
      if config.scala.isDefined then
        val segs = folder split '-'

        if segs.length == 1 then folder
        else
          segs map {
            case "1" => "umuna_a"
            case "2" => "maikadua_a"
            case "3" => "maikatlo_a"
            case s   => s
          } mkString "_"
      else folder
    val outdir = output resolve book

    message(s"Creating directory $outdir for book '$bookName'")
    Files.createDirectories(outdir)

    val chapters = list(d).filter(_.getFileName.toString.endsWith(".md"))
    val chapterList =
      chapters map { f =>
        message(s"Reading markdown file $f")

        if (!Files.isReadable(f)) problem(s"File $f is unreadable")

        val chapter = f.getFileName.toString.dropRight(3).toInt.toString
        val in = Files.readString(f)
        val outfile = outdir resolve (if config.scala.isDefined then s"${book}_$chapter.scala" else chapter)
        val out = new PrintWriter(outfile.toString)
        val (contents, verses) = transform(in)

        message(s"Writing to file $outfile")

        if config.scala.isDefined then
          out.println(s"package ${config.scala.get}.$book")
          out.println()
          out.println(s"val ${book}_$chapter =")
          out.print("\"\"\"")

        out.println(s"""<div class="prose${config.clas.map(' ' +: _).getOrElse("")}">""")
        out.println(contents)
        out.print("</div>")

        if config.scala.isDefined then out.print("\"\"\"")

        out.close()
        verses
      }

    if config.scala.isDefined then
      Files.writeString(
        outdir resolve "book.scala",
        s"""package ${config.scala.get}.$book
            |
            |import collection.immutable.ArraySeq
            |
            |val book = ArraySeq(${chapterList.zipWithIndex map ((v, c) => s"(${book}_${c + 1}, $v)") mkString ", "})
            |""".stripMargin,
      )
  }
