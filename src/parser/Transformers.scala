package smack.parser

import scala.util.parsing.combinator._

trait Transformers extends RegexParsers {

  // Common

  def toString(a: Any): String = a match {
    case Some(a)      => toString(a)
    case None         => ""
    case s: String    => s
    case l: List[Any] => l.foldLeft("") { _ + toString(_) }
    case ~(a, b)      => toString(a) + toString(b)
  }

  def toList[T](a: Any): List[T] = a match {
    case l: List[Any] => l.asInstanceOf[List[T]]
  }

  // Specific

  def toSmack(a: Any): SmackFile = a match {
    case a: List[Any] => new SmackFile(a.asInstanceOf[List[Module]])
  }

  def toModule(a: Any): Module = a match {
    case ~(~(name: String, parent: Option[String]), body: ModuleBlock) => Module(name, parent, body)
  }

  def toModuleBlock(a: Any) = a match {
    case ~(Some(vars: List[Any]), rules: List[Any]) => ModuleBlock(vars.asInstanceOf[List[ModuleVar]], rules.asInstanceOf[List[Rule]])
    case ~(None, rules: List[Any])                  => ModuleBlock(List[ModuleVar](), rules.asInstanceOf[List[Rule]])
  }

  def toModuleVar(a: Any) = a match {
    case ~(name: String, value: String) => ModuleVar(name, value)
  }

  def toRule(a: Any) = a match {
    case ~(selector: String, styles: List[Any]) => Rule(selector, styles.asInstanceOf[List[Style]])
  }

  def toStyle(a: Any) = a match {
    case ~(property: String, value: String) => Style(property, value)
  }
}