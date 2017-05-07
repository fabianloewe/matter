package de.hyronx.matter.library

import fastparse.all._

trait ParserBuilder {
  def apply: P[_]
  def map(a: Any): Any
}

object FastparseWrapper {
  private def flattenToList(t: Product): List[_] = {
    val r = t.productIterator.toList.map { x: Any ⇒
      x match {
        case nested: Product ⇒ flattenToList(nested)
        case _               ⇒ x
      }
    }

    val s = r.foldLeft(List.empty[Any]) { (left: List[_], right: Any) ⇒
      right match {
        case nested: Seq[_] ⇒ left ++ nested
        case _              ⇒ left :+ right
      }
    }
    println(s"FastparseWrapper:flattenToList! Result: $s")
    s
  }

  def concat[_](
    ps: Array[P[_]]
  ): P[_] = {
    var parser: P[_] = ps.head
    ps.tail.foreach { p ⇒ parser = parser ~ p }
    println(s"FastparseWrapper:concat! Parser: $parser")
    parser map { case tup: Product ⇒ flattenToList(tup) }
  }

  def or(
    ps: Array[P[_]]
  ): P[_] = {
    var parser: P[_] = ps.head
    ps.tail.foreach { p ⇒ parser = parser | p }
    parser
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

  def range(from: String, to: String) = {
    println(s"FastparseWrapper:range($from, $to)")
    if (from.length == 1 && to.length == 1)
      P(CharIn(from.head to to.head).!)
    else
      P(CharIn(from, to).!)
  }

  def literal(lit: String) = {
    println(s"FastparseWrapper:literal($lit)")
    P(lit).!
  }

  def buildParser(pb: ParserBuilder) = {
    val parser = P(pb.apply map { x ⇒
      println(s"FastparseWrapper:buildParser! Map variable: $x")
      pb.map(x)
    })
    parser
  }

  def mkString(obj: Any): String = obj match {
    case Some(seq: Seq[_]) ⇒ seq.map(mkString(_)).mkString
    case Some(value)       ⇒ value.toString
    case seq: Seq[_]       ⇒ seq.map(mkString(_)).mkString
    case _                 ⇒ obj.toString

  }

  def mkString(product: Product): String = product.productIterator.map { elem ⇒
    elem match {
      case Some(seq: Seq[_])   ⇒ seq.mkString
      case Some(prod: Product) ⇒ mkString(prod)
      case Some(value)         ⇒ value.toString
      case prod: Product       ⇒ mkString(prod)
      case seq: Seq[_]         ⇒ seq.mkString
      case _                   ⇒ elem.toString
    }
  }.toList.mkString

  def toFloat(p: P[_]): P[Float] = p.! map { x ⇒
    java.lang.Float.valueOf(x).floatValue
  }

  def map(p: P[_], f: Any ⇒ P[_]) = p map f
}
