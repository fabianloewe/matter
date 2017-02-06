package de.hyronx.matter.compiler.lexers

import de.hyronx.matter.compiler.tokens._

trait BehaviorLexer extends BaseLexer {
  def on = "on" ^^ (_ ⇒ ON)
  def lookup = "lookup" ^^ (_ ⇒ LOOKUP)
  def arrowLeft = "->" ^^ (_ ⇒ ARROW_LEFT)
  def in = "in" ^^ (_ ⇒ IN)

  def behavior: Parser[Token] = on | lookup | arrowLeft | in
}
