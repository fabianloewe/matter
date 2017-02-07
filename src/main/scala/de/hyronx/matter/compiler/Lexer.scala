package de.hyronx.matter.compiler

import de.hyronx.matter.compiler.tokens._
import de.hyronx.matter.compiler.lexers._

object Lexer extends ContentLexer with BehaviorLexer {
  import fastparse.all._

  val tokens: P[List[Token]] = {
    (content | behavior | base).rep(1) map { list ⇒
      processIndentation(list)
    }
  }

  def apply(
    code: String
  ): Either[LexerError, List[Token]] = tokens.parse(code) match {
    case Parsed.Failure(lastParser, index, extra) ⇒ Left(LexerError(lastParser + s" at ${index}!"))
    case Parsed.Success(result, _)                ⇒ Right(result)
  }
}
