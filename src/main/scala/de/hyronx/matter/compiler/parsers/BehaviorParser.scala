package de.hyronx.matter.compiler.parsers

import de.hyronx.matter.compiler.ast._

trait BehaviorParser extends BaseParser {
  import fastparse.noApi._
  import ignoreWhitespaces._

  val onCommand = {
    P("on" ~/ variable ~ "->" ~ operations) map {
      case (newVar, command) ⇒
        OnCommand(newVar, command)
    }
  }

  val lookupOne = P("lookup" ~/ scopedIdentifier) map { id ⇒ Lookup(id) }

  val lookupList = {
    P("lookup:" ~/ Indentation.count ~ scopedIdentifier.rep(1)) map {
      case (_, seq) ⇒ LookupList(seq.toList)
    }
  }

  val lookup: P[AST] = lookupOne | lookupList

  private val operations: P[AST] = onCommand | lookup

  val inDo = P("in" ~/ scopedIdentifier ~ operations) map {
    case (id, op) ⇒ InDo(id, op)
  }

  val behavior: P[Seq[AST]] = (inDo | operations).rep.log()
}
