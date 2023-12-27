package io.github.edadma.text2html

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec

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
  var firstParagraph = true

  def tag(name: String, body: String, cls: String | Null = null): Unit =
    if buf.nonEmpty then buf += '\n'

    buf ++= s"<$name${if cls eq null then "" else s""" class="$cls""""}>$body</$name>"

  def add(text: String): Unit =
    if par.nonEmpty then par += '\n'

    par ++= text

  def paragraph(): Unit =
    if par.nonEmpty then
      tag("p", par.toString, if firstParagraph then "m-0" else "indent-8 m-0")
      par.clear()
      firstParagraph = false

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
      firstParagraph = true
      tag(s"h$count", heading)
      transform(r3)
    else
      val (line, r1) = consumeLine(r)
      val (next, r2) = consumeLine(r1)

      if next.nonEmpty && next.forall(_ == '=') then
        paragraph()
        firstParagraph = true
        tag("h1", line)
        transform(r2)
      else if next.nonEmpty && next.forall(_ == '-') then
        paragraph()
        firstParagraph = true
        tag("h2", line)
        transform(r2)
      else if line.head.isDigit then
        val (verse, rest) = line.span(_.isDigit)

        add(s"<sup>$verse</sup>${rest.trim}")
        transform(r1)
      else
        add(line)
        transform(r1)
  end transform

  transform(CharReader.fromString(md))
end transform
