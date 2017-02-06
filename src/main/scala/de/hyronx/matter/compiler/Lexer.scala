package de.hyronx.matter.compiler

import de.hyronx.matter.compiler.tokens._
import de.hyronx.matter.compiler.lexers._

object Lexer extends ContentLexer with BehaviorLexer {
  def tokens: Parser[List[Token]] = {
    phrase(rep1(content | behavior | base)) ^^ {
      list ⇒
        println(list)
        processIndentation(list)
    }
  }

  def apply(
    code: String
  ): Either[LexerError, List[Token]] = parse(tokens, code) match {
    case NoSuccess(msg, next)  ⇒ Left(LexerError(msg + s" at ${next.pos}!"))
    case Success(result, next) ⇒ Right(result)
  }
}
