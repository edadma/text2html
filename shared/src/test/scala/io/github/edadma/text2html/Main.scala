package io.github.edadma.text2html

import io.github.edadma.char_reader.CharReader
import pprint.*

import scala.annotation.tailrec

@main def run(): Unit =
  val input = """
    |# testing
    |
    |asdf
    |
    |##next heading
    |
    |zxcv
    |dfhg
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

      if par.nonEmpty then par += '\n'

      par ++= line
      transform(r1)
  end transform

  transform(CharReader.fromString(md))
end transform
