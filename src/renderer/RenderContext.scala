package smack.renderer

import smack.parser._

private class RenderContext(val parentSelector: String, val module: Module, val renderSettings: RenderSettings) {

  val vars: Map[String, String] = module.body.vars.foldLeft(Map[String, String]()) { (acc, a) => a match {
    case v: ModuleVar => acc ++ Map(v.name -> v.value)
  }}

  val rules: Map[String, Map[String, String]] = module.body.rules.foldLeft(Map[String, Map[String, String]]()) { (acc, rule) =>
    val styles = rule.styles.foldLeft(Map[String, String]()) { (acc, s) => acc ++ Map(s.property -> s.value) }
    acc ++ Map(rule.selector -> styles)
  }

  def selector(child: String) = parentSelector match {
    case p if p.size > 0 => p + " " + child
    case _               => child
  }

  def getVarValue(varName: String): String = {
    if (varName.contains("@")) {
      val name = varName.replace("@", "")
      if (name.contains(".")) getVarValue(getRuleTreeValue(name))
      else vars.get(name) match {
        case Some(v: String) => v
        case None            => ""
      }
    }
    else varName
  }

  def resolveValue(v: String) = {
    if (v.contains("@")) getVarValue(v)
    else v
  }

  private def getRuleTreeValue(path: String) = {
    val segs = namespaceSplit(path)
    rules.get(segs(0)) match {
      case None         => ""
      case Some(styles) => {
        styles.get(segs(1)) match {
          case None        => ""
          case Some(value) => value
        }
      }
    }
  }

  private def namespaceSplit(s: String) = {
    s.split("""\.""") match {
      case l if l.size == 0 => Array(s)
      case l                => l
    }
  }

}