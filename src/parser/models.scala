package smack.parser

case class ModuleBlock(vars: List[ModuleVar], val rules: List[Rule])
case class ModuleVar(name: String, var value: String)

case class Rule(selector: String, styles: List[Style])
case class Style(property: String, value: String)