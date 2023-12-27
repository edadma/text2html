package io.github.edadma.text2html

import io.github.edadma.char_reader.CharReader
import pprint.*

import scala.annotation.tailrec

@main def run(): Unit =
  val input = """
    |testing
    |=======
    |
    |asdf
    |
    |next heading
    |------------
    |
    |zxcv
    |dfhg
    |""".stripMargin

  pprintln(input)
  pprintln(transform(input))
