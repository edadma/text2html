package io.github.edadma.text2html

import java.nio.file.{Files, Paths}
import pprint.*

import java.io.PrintWriter

@main def run(): Unit =
  val root = Paths.get("test/input").toAbsolutePath
  val output = Paths.get("test/output").toAbsolutePath

  println(s"Root directory $root")

  if (!Files.isReadable(root))
    problem(s"Directory $root is unreadable")

  if (!Files.exists(output))
    println(s"Creating directory $output")
    Files.createDirectories(output)

  list(root).filter(p => isDir(p) && !p.getFileName.toString.startsWith(".")) foreach { d =>
    println(s"Processing directory $d")

    val book = d.getFileName.toString
    val bookName = book split '_' map (_.capitalize) mkString " "

    println(s"Book '$bookName'")

    list(d).filter(_.getFileName.toString.endsWith(".md")) foreach { f =>
      println(s"Reading markdown file $f")

      if (!Files.isReadable(f))
        problem(s"File $f is unreadable")

      val chapter = f.getFileName.toString.dropRight(3)
      val in = Files.readString(f)
      val outfile = output.resolve(s"$book-$chapter.html").toString
      val out = new PrintWriter(outfile)

      println(s"Writing to file $outfile")

      out.close()
    }
  }
