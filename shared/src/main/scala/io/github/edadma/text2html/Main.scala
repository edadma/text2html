package io.github.edadma.text2html

import io.github.edadma.char_reader.CharReader

import java.nio.file.{Files, Paths}
import pprint.*

import java.io.PrintWriter
import scala.annotation.tailrec

@main def run(): Unit =
  val root = Paths.get("test/input").toAbsolutePath
  val output = Paths.get("test/output").toAbsolutePath

  println(s"Root directory $root")

  if (!Files.isReadable(root)) problem(s"Directory $root is unreadable")

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

      if (!Files.isReadable(f)) problem(s"File $f is unreadable")

      val chapter = f.getFileName.toString.dropRight(3)
      val in = Files.readString(f)
      val outfile = output.resolve(s"$book-$chapter.html")

      if (!Files.isWritable(outfile)) problem(s"File $outfile is unwritable")

      val out = new PrintWriter(outfile.toString)

      println(s"Writing to file $outfile")
      out.println(transform(in))
      out.close()
    }
  }

def transform(md: String): String =
  @tailrec
  def skipUntilAfter(r: CharReader, c: Char): CharReader =
    if r.eoi then r
    else if r.ch == c then r.next
    else skipUntilAfter(r.next, c)

  @tailrec
  def skipSpace(r: CharReader): CharReader =
    if r.ch == ' ' then skipSpace(r.next)
    else r

  @tailrec
  def skipRepeating(r: CharReader, c: Char, count: Int = 0): (Int, CharReader) =
    if r.ch == c then skipRepeating(r.next, c, count + 1)
    else (count, r)

  @tailrec
  def consumeLine(r: CharReader, buf: StringBuilder = new StringBuilder): (String, CharReader) =
    if r.eoi then (buf.toString, r)
    else if r.ch == '\n' then (buf.toString, r.next)
    else
      buf += r.ch
      consumeLine(r.next, buf)

  val buf = new StringBuilder
  val par = new StringBuilder

  def tag(name: String, body: String): Unit =
    if buf.nonEmpty then buf += '\n'

    buf ++= s"<$name>$body</$name>"

  def add(text: String): Unit =
    if par.nonEmpty then par += '\n'

    par ++= text

  def paragraph(): Unit =
    if par.nonEmpty then
      tag("p", par.toString)
      par.clear()

  @tailrec
  def transform(r: CharReader): String =
    if r.eoi then
      paragraph()
      buf.toString
    else if r.ch == '\n' then
      paragraph()
      transform(r.next)
    else if r.ch == '#' then
      val (count, r1) = skipRepeating(r, '#')
      val r2 = skipSpace(r1)
      val (heading, r3) = consumeLine(r2)

      paragraph()
      tag(s"h$count", heading)
      transform(r3)
    else
      val (line, r1) = consumeLine(r)
      val (next, r2) = consumeLine(r1)

      if next.nonEmpty && next.forall(_ == '=') then
        paragraph()
        tag("h1", line)
        transform(r2)
      else if next.nonEmpty && next.forall(_ == '-') then
        paragraph()
        tag("h2", line)
        transform(r2)
      else
        add(line)
        transform(r1)
  end transform

  transform(CharReader.fromString(md))
end transform
