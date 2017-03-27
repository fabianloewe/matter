package de.hyronx.matter.support

import fastparse.all._

trait ParserBuilder {
  def apply[T]: P[T]
}

object FastparseWrapper {
  def concat[T](
    ps: Seq[P[T]]
  ): P[_] = ps.headOption match {
    case Some(p) ⇒ p ~ concat(ps.tail)
    case None    ⇒ Pass
  }

  def or(
    ps: Seq[P[_]]
  ): P[_] = ps.headOption match {
    case Some(p) ⇒ p | or(ps.tail)
    case None    ⇒ Pass
  }

  def rep[T](
    p: P[T],
    min: Int = 0,
    sep: P[_] = Pass,
    max: Int = 0,
    exactly: Int = -1
  ) = p.rep(min, sep, max, exactly)

  def opt[T](p: P[T]) = p.?

  def range(from: Char, to: Char) = CharIn(from to to)

  def literal(lit: String) = P(lit)

  def buildParser(pb: ParserBuilder) = {
    P(pb.apply)
  }
}
