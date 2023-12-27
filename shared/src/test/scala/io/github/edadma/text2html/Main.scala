package io.github.edadma.text2html

import io.github.edadma.char_reader.CharReader
import pprint.*

import scala.annotation.tailrec

@main def run(): Unit =
  val input = """
    |testing
    |
    |asdf
    |""".stripMargin

  pprintln(input)
  pprintln(transform(input))

def transform(md: String): String =
  @tailrec
  def skipUntilAfter(r: CharReader, c: Char): CharReader =
    if r.eoi then r
    else if r.ch == c then r.next
    else skipUntilAfter(r.next, c)

  @tailrec
  def consumeLine(r: CharReader, buf: StringBuilder = new StringBuilder): (String, CharReader) =
    if r.eoi then (buf.toString, r)
    else if r.ch == '\n' then (buf.toString, r.next)
    else
      buf += r.ch
      consumeLine(r.next, buf)

  val buf = new StringBuilder
  val par = new StringBuilder

  def paragraph(): Unit =
    if par.nonEmpty then
      if buf.nonEmpty then buf += '\n'

      buf ++= s"<p>${par.toString}</p>"
      par.clear()

  @tailrec
  def transform(r: CharReader): String =
    if r.eoi then
      paragraph()
      buf.toString
    else if r.ch == '\n' then
      paragraph()
      transform(r.next)
//    else if r.ch == '#'
    else
      val (line, r1) = consumeLine(r)

      if par.nonEmpty then par += '\n'

      par ++= line
      transform(r1)
  end transform

  transform(CharReader.fromString(md))
end transform
