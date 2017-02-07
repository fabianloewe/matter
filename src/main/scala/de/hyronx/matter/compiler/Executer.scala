package de.hyronx.matter.compiler

import de.hyronx.matter.compiler.parsers.BaseParser
import de.hyronx.matter.compiler.ast._

object ParserGenerator extends BaseParser {
  def apply(list: List[AST]): Parser[Any] = {
    list.headOption match {
      case Some(ast) ⇒
        ast match {
          case option: Option ⇒
            opt(ParserGenerator(option.definitions))
          case repeat: Repeat ⇒
            rep(ParserGenerator(repeat.definitions))
          case repeatOne: RepeatOne ⇒
            rep1(ParserGenerator(repeatOne.definitions))
          case lit: Literal ⇒
            accept(lit.string, { case n if n == lit.string ⇒ n })
          case definition ⇒
            err(s"Invalid definition: $definition")
        }
      case None ⇒
        success(None)
    }
  }
}

object Executer extends BaseParser {
  def generate(
    openContainer: ContainerTree
  ): List[(String, ParserGenerator.Parser[Any])] = openContainer match {
    case container: Container ⇒
      generateParser(container, container.content)
    case pseudo: PseudoContainer ⇒
      generate(pseudo.ancestor)
  }

  def generateParser(
    container: Container,
    content: Types.ContentMap
  ): List[(String, ParserGenerator.Parser[Any])] = {
    content.headOption match {
      case Some((variable, ast)) ⇒
        (variable, ParserGenerator(ast)) :: generateParser(container, content.tail)
      case None ⇒
        List()
    }
  }

  def apply(id: Identifier, parent: ContainerTree) = {
    parent.find(id) match {
      case Some(openContainer) ⇒
        generate(openContainer).map {
          case (variable, parser) ⇒
            parser
        }.reduceLeft(_ ~ _)
      case None ⇒
        err(s"Container $id not found")
    }

  }

}
