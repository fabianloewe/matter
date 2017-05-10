package de.hyronx.matter

import java.io.File

sealed trait CompilerOp
case object CompileOp extends CompilerOp
case object NewOp extends CompilerOp
case object RunOp extends CompilerOp

case class Config(
  op: CompilerOp = CompileOp,
  files: Seq[File] = Seq(),
  outDir: File = new File("."),
  buildDir: File = new File("build"),
  packageName: String = "",
  vendor: Option[String] = None,
  version: String = "0.0.1",
  useGit: Boolean = true
)
