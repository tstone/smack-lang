package smack.parser

import scala.util.parsing.combinator._

object Parser extends RegexParsers with Transformers {
  override val whiteSpace = """[ \t]+""".r

//  def file = rep(moduleInclude | module)
  def file = rep(module) ^^ toSmack

  def moduleInclude = "@module-include" ~ "(" ~> identifier ~ opt(varAssingmentList) <~ ")"
  def varAssingmentList = "," ~> rep(moduleVarAssignment <~ rep(eol))
  def moduleVarAssignment = "@" ~> identifier ~ moduleVarValue

  def module = "@module " ~> identifier ~ opt(moduleLineage) ~ moduleBlock ^^ toModule
  def moduleLineage = ">>" ~> namespacedIdentifier
  def moduleBlock = "{" ~ rep(eol) ~> opt(varList) ~ ruleList <~ rep(eol) ~ "}" ^^ toModuleBlock
  def ruleList = rep(rule <~ rep(eol)) ^^ toList[Rule]
  def varList = rep(moduleVar <~ rep(eol))
  def moduleVar = "@" ~> identifier ~ moduleVarValue ^^ toModuleVar

  def rule = selector ~ ruleBlock ^^ toRule
  def ruleBlock = "{" ~ rep(eol) ~> rep(style) <~ rep(eol) ~ "}" ^^ toList[Style]
  def style = property ~ propValue ^^ toStyle

  def moduleVarValue = "=" ~> (varValue | constValue) <~ (";" | eol) ^^ toString
  def propValue = ":" ~> (varValue | constValue) <~ (";" | eol) ^^ toString
  def constValue = propChar ~ rep(propChar)
  def varValue = "@" ~ namespacedIdentifier
  def propChar = """[^;\n]""".r

  def namespacedIdentifier = identifier ~ rep("." ~ identifier) ^^ toString
  def identifier = (lowerLetter | "-") ~ rep(lowerLetter | digit | "_" | "-") ^^ toString
  def property = lowerLetter ~ rep(lowerLetter | "-") ^^ toString

  def selector = (tagSelector | classSelector | idSelector | attrSelector | wildcard | pseudoClassSelector) ^^ toString
  def pseudoClassSelector = ":" ~ rep1(lowerLetter)
  def wildcard = "*"
  def attrSelector = "[" ~ """[^\]]""".r ~ "]"
  def idSelector = "#" ~ identifier ~ opt(attrSelector) ~ opt(pseudoClassSelector)
  def classSelector = "." ~ identifier ~ opt(attrSelector) ~ opt(pseudoClassSelector)
  def tagSelector = rep1(letter) ~ opt(attrSelector) ~ opt(pseudoClassSelector)

  def lowerLetter = "[a-z]".r
  def letter = "[A-Za-z]".r
  def digit = "[0-9]".r
  def eol = """(\r?\n)+""".r

  def apply(code: String) = parseAll(file, code.trim) match {
    case Success(result, _) => result
    case failure: NoSuccess => sys.error(failure.msg)
  }
}