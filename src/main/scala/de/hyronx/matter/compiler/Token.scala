package de.hyronx.matter.compiler.tokens

sealed trait Token

sealed trait NameToken extends Token
case class IDENTIFIER(string: String) extends NameToken
case class VARIABLE(string: String) extends NameToken
case class LITERAL(string: String) extends NameToken
case class CHAR_LOWER(character: String) extends Token
case class CHAR_UPPER(character: String) extends Token
case class CHAR_NUM(character: String) extends Token

case class INDENTATION(spaces: Int) extends Token
case object WHITESPACE extends Token
case object ENDLINE extends Token
case object COLON extends Token
case object DOT extends Token
case object LESSTHAN extends Token
case object COMMA extends Token
case object INDENT extends Token
case object DEDENT extends Token

case object PARENTHESIS_LEFT extends Token
case object PARENTHESIS_RIGHT extends Token
case object CURLY_BRACKET_LEFT extends Token
case object CURLY_BRACKET_RIGHT extends Token
case object SQUARE_BRACKET_LEFT extends Token
case object SQUARE_BRACKET_RIGHT extends Token
case object ASSIGN extends Token
case object ASTERISK extends Token
case object PLUS extends Token
case object SPACE extends Token
case object DASH extends Token

case object ON extends Token
case object LOOKUP extends Token
case object ARROW_LEFT extends Token
case object IN extends Token
