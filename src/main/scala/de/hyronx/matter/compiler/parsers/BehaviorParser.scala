package de.hyronx.matter.compiler.parsers

import de.hyronx.matter.compiler.tokens._
import de.hyronx.matter.compiler.ast._

trait BehaviorParser extends BaseParser {
  def onCommand: Parser[OnCommand] = {
    log(ON ~ LOOKUP ~ ARROW_LEFT ~ lookup)("on <command>") ^^ {
      case _ ~ _ ~ _ ~ op ⇒ OnCommand("lookup", op)
    }
  }

  def lookupOne: Parser[Lookup] = {
    LOOKUP ~ identifier ^^ {
      case _ ~ id ⇒ Lookup(id)
    }
  }

  def lookupList: Parser[LookupList] = {
    LOOKUP ~ COLON ~ INDENT ~ rep1(identifier) ^^ {
      case _ ~ _ ~ _ ~ list ⇒
        LookupList(list)
    }
  }

  def lookup: Parser[AST] = lookupOne | lookupList

  private def operations: Parser[AST] = onCommand | lookup

  def inDo: Parser[InDo] = {
    IN ~ identifier ~ operations ^^ {
      case _ ~ id ~ op ⇒ InDo(id, op)
    }
  }

  def behavior: Parser[List[AST]] = {
    rep1(inDo | operations) ^^ (list ⇒ list)
  }
}
