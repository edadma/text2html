package io.github.edadma.text2html

import java.nio.file.{Files, Paths}

@main def run(): Unit =
  val root = Paths.get("test").toAbsolutePath

  println(s"Root directory $root")

  Files.list(root)
