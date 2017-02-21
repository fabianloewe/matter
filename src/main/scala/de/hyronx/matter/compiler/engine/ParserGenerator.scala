package de.hyronx.matter.compiler.engine

import scala.collection.mutable.Map

import de.hyronx.matter.compiler.parsers.BaseParser
import de.hyronx.matter.compiler.types.Type
import de.hyronx.matter.compiler.ast._

object ParserGenerator {
  import fastparse.all._

  var container: ContainerTree = BaseContainer

  def apply(contentMap: Types.ContentMap, container: ContainerTree) = {
    this.container = container

    // Generate parser from content definitions
    val parserMap = contentMap map {
      case (key, value) ⇒ key → generate(value).log()
    }
    val result = () ⇒ {
      val resultMap = Map.empty[String, Any]

      // Combine all parsers to one
      parserMap.foldLeft(Pass.asInstanceOf[P0]) {
        case (left, (key, parser)) ⇒
          println(s"~ ParserGenerator: Current key = $key, left = $left, right = $parser")
          P(left ~ parser) map {
            case value: Any ⇒
              println(s"~ Got some value: $value")
              resultMap(key) = value
            case something ⇒
              println(s"~ ParserGenerator: Current match = $something, key = $key, left = $left")
          }
      } map (_ ⇒ resultMap)
    }
    result
  }

  def generate(astSeq: Seq[AST]): P[Any] = {
    astSeq.headOption match {
      case Some(ast) ⇒ ast match {
        case Optional(defs) ⇒
          generate(defs).?
        case Repeat(defs) ⇒
          generate(defs).rep
        case RepeatOne(defs) ⇒
          generate(defs).rep(1)
        case Regex(defs) ⇒
          generate(defs) // ~ generate(astSeq.tail)
        case Range(from, to) ⇒
          CharIn(from to to) ~ generate(astSeq.tail)
        case Literal(string) ⇒
          string.! ~ generate(astSeq.tail)
        case id: Identifier ⇒
          this.container.find(id) match {
            case Some(container) ⇒ P(container.parser ~ generate(astSeq.tail)) map {
              // TODO: do something with the first part
              case (_, second) ⇒
                println(s"~ Container found")
                second
            }
            case None ⇒
              println(s"~ Container not found")
              Fail
          }
        case definition ⇒
          Fail
      }
      case None ⇒
        Pass
    }
  }
}
