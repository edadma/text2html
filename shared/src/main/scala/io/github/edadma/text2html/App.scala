package io.github.edadma.text2html

import scala.language.postfixOps
import java.nio.file.{Files, Paths}
import java.io.PrintWriter

def App(config: Config): Unit =
  val root = Paths get "test/input" toAbsolutePath
  val output = Paths get "test/text" toAbsolutePath

  println(s"Root directory $root")

  if (!Files.isReadable(root)) problem(s"Directory $root is unreadable")

  list(root).filter(p => isDir(p) && !p.getFileName.toString.startsWith(".")) foreach { d =>
    println(s"Processing directory $d")

    val book = d.getFileName.toString
    val bookName = book split '_' map (_.capitalize) mkString " "
    val outdir = output resolve book

    println(s"Creating directory $outdir for book '$bookName'")
    Files.createDirectories(outdir)

    list(d).filter(_.getFileName.toString.endsWith(".md")) foreach { f =>
      println(s"Reading markdown file $f")

      if (!Files.isReadable(f)) problem(s"File $f is unreadable")

      val chapter = f.getFileName.toString.dropRight(3).toInt.toString
      val in = Files.readString(f)
      val outfile = outdir resolve chapter
      val out = new PrintWriter(outfile.toString)

      println(s"Writing to file $outfile")
      out.println(
        """<div class="prose prose-h1:text-gray-400 prose-h1:font-fondamento prose-h1:font-normal prose-h2:text-gray-400 prose-h3:text-gray-400 prose-p:text-gray-400 prose-p:m-0">""",
      )
      out.println(transform(in))
      out.print("</div>")
      out.close()
    }
  }
