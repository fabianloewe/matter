package de.hyronx.matter.library

import fastparse.all._

trait MatterBased {
  def getParser(): P[_]
}

object FastparseWrapper {
  private var debug = false

  def setDebug(b: Boolean): Unit = debug = b

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
    if (debug) System.out.println(s"FastparseWrapper:flattenToList! Result: $s")
    s
  }

  def concat[_](
    ps: Array[P[_]]
  ): P[_] = {
    var parser: P[_] = ps.head
    ps.tail.foreach { p ⇒ parser = parser ~ p }
    if (debug) System.out.println(s"FastparseWrapper:concat! Parser: $parser")
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
    if (debug) System.out.println(s"FastparseWrapper:rep($min, $max)")
    if (sep == null) {
      p.rep(min, Pass, max, exactly)
    } else {
      p.rep(min, sep.getOrElse(Pass), max, exactly)
    }
  }

  def opt(p: P[_]) = p.?

  def range(from: String, to: String) = {
    if (debug) System.out.println(s"FastparseWrapper:range($from, $to)")
    if (from.length == 1 && to.length == 1)
      P(CharIn(from.head to to.head).!)
    else
      P(CharIn(from, to).!)
  }

  def literal(lit: String) = {
    if (debug) System.out.println(s"FastparseWrapper:literal($lit)")
    P(lit).!
  }

  /*
  def buildParser(pb: ParserBuilder) = {

    val parser = P(pb.apply map { x ⇒
      println(s"FastparseWrapper:buildParser! Map variable: $x")
      pb.map(x)
    })

    //val parser = pb.apply
    println(s"FastparseWrapper:buildParser! Parser: $parser")
    parser
  }

  def mkString(obj: Any): String = {
    val result = obj match {
      case Some(seq: Seq[_]) ⇒ seq.map(mkString(_)).mkString
      case Some(value)       ⇒ value.toString
      case seq: Seq[_]       ⇒ seq.map(mkString(_)).mkString
      case _                 ⇒ obj.toString
    }

    println(s"FastparseWrapper:mkString($obj)! Result: $result")
    result
  }
  *
  */

  def mkString(obj: Any): String = {
    val result = obj match {
      case product: Product ⇒
        product.productIterator.toList.map { elem ⇒
          elem match {
            case Some(seq: Seq[_]) ⇒ seq.mkString
            case Some(prod: Product) ⇒ mkString(prod)
            case Some(value) ⇒ value.toString
            case prod: Product ⇒ mkString(prod)
            case seq: Seq[_] ⇒ seq.mkString
            case _ ⇒ elem.toString
          }
        }.mkString
      case seq: Seq[_] ⇒ seq.map(mkString(_)).mkString
      case _ ⇒ obj.toString
    }
    if (debug) System.out.println(s"FastparseWrapper:mkString($obj)! Result: $result")
    result
  }

  def toFloat(p: P[_]): P[Float] = p.! map { x ⇒
    java.lang.Float.valueOf(x).floatValue
  }

  def map(p: P[_], f: Any ⇒ P[_]) = {
    if (debug) System.out.println(s"FastparseWrapper:map($p, $f)")
    p map f
  }

  def println(obj: Any) = System.out.println(obj.toString)

  def log(p: P[_]) = p.log()
}
