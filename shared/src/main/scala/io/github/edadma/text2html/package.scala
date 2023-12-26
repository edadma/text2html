package io.github.edadma.text2html

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

def isFile(p: Path): Boolean = Files.isRegularFile(p) && Files.isReadable(p)

def isDir(p: Path): Boolean = Files.isDirectory(p) && Files.isReadable(p)

def canCreate(p: Path): Boolean = Files.isDirectory(p.getParent) && Files.isWritable(p.getParent)

def problem(msg: String): Nothing = {
  Console.err.println(msg)
  sys.exit(1)
}

def list(dir: Path): List[Path] = Files.list(dir).iterator.asScala.toList sortBy (_.getFileName.toString)
