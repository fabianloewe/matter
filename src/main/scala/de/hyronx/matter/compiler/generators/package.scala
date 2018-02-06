package de.hyronx.matter.compiler

package object generators {
  private[generators] val PARSER_CLASS = "fastparse/core/Parser"
  private[generators] val PARSER_TYPE = s"L${PARSER_CLASS};"
  private[generators] val STRING_CLASS = "java/lang/String"
  private[generators] val STRING_TYPE = s"L${STRING_CLASS};"
  private[generators] val STRING_BUILDER_CLASS = "java/lang/StringBuilder"
  private[generators] val STRING_BUILDER_TYPE = s"L${STRING_BUILDER_CLASS};"
  private[generators] val INTERFACE_CLASS = "de/hyronx/matter/library/MatterBased"
  private[generators] val WRAPPER_CLASS = "de/hyronx/matter/library/FastparseWrapper"
  private[generators] val OBJECT_TYPE = "Ljava/lang/Object;"
  private[generators] val SCALA_FUNCTION1_CLASS = "scala/Function1"
  private[generators] val SCALA_FUNCTION1_TYPE = s"L$SCALA_FUNCTION1_CLASS;"
  private[generators] val SCALA_LIST_CLASS = "scala/collection/Seq"
  private[generators] val SCALA_LIST_TYPE = s"L$SCALA_LIST_CLASS;"

  private[generators] val OPT_WRAPPER_SIG = s"($PARSER_TYPE)$PARSER_TYPE"
  private[generators] val REP_WRAPPER_SIG = s"(${PARSER_TYPE}ILscala/Option;II)$PARSER_TYPE"

  private[generators] val DefaultVarPattern = "\\$(\\d)".r
}
