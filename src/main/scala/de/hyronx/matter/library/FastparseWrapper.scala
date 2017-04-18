package de.hyronx.matter.library

import fastparse.all._

trait ParserBuilder[T] {
  def apply: P[T]
}

object FastparseWrapper {
  def concat[_](
    ps: Array[P[_]]
  ): P[_] = {
    var parser: P[_] = ps.head
    ps.takeRight(ps.size - 1).foreach { p ⇒ parser = parser ~ p }
    parser.!
  }

  def or(
    ps: Array[P[_]]
  ): P[_] = {
    var parser: P[_] = ps.head
    ps.takeRight(ps.size - 1).foreach { p ⇒ parser = parser | p }
    parser.!
  }

  def rep(
    p: P[_],
    min: Int = 0,
    sep: Option[P[_]] = None,
    max: Int = 0,
    exactly: Int = -1
  ) = {
    println(s"FastparseWrapper:rep($min, $max)")
    if (sep == null) {
      p.rep(min, Pass, max, exactly)
    } else {
      p.rep(min, sep.getOrElse(Pass), max, exactly)
    }
  }

  def opt(p: P[_]) = p.?

  def range(from: Char, to: Char) = {
    println(s"FastparseWrapper:range($from, $to)")
    P(CharIn(from to to).!)
  }

  def literal(lit: String) = {
    println(s"FastparseWrapper:literal($lit)")
    P(lit).!
  }

  def buildParser[T](pb: ParserBuilder[T]) = {
    val parser = pb.apply
    println(s"FastparseWrapper:buildParser! Parser: $parser")
    parser
  }
}
