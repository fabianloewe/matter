package de.hyronx.matter.compiler.lexers

import de.hyronx.matter.compiler.tokens._

trait BehaviorLexer extends BaseLexer {
  import fastparse.noApi._
  import whitespace._

  val on = P("on") map (_ ⇒ ON)
  val lookup = P("lookup") map (_ ⇒ LOOKUP)
  val arrowLeft = P("->") map (_ ⇒ ARROW_LEFT)
  val in = P("in") map (_ ⇒ IN)

  val behavior: P[Token] = on | lookup | arrowLeft | in
}
