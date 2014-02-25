package smack.renderer

import smack.parser._

object Renderer {

  def render(smack: SmackFile, rs: RenderSettings = new RenderSettings): String = {
    val modules = smack.modules.foldLeft(""){ (acc: String, m: Module) =>
      val context = new RenderContext(moduleSelector(m), m, rs)
      acc + render(m, context)
    }
    modules.trim
  }

  private def render(m: Module, context: RenderContext): String = {
    val rs = context.renderSettings
    rs.beforeModule +
      m.body.rules.foldLeft("")(_ + render(_, context)) +
    rs.afterModule
  }

  private def moduleSelector(m: Module): String = m.parent match {
    case Some(p) => "." + p.replaceAll("""\.""", "-") + "-" + m.name
    case None    => m.name
  }

  private def render(r: Rule, context: RenderContext): String = {
    val rs = context.renderSettings
    rs.beforeRule + context.selector(r.selector) + rs.afterRuleSelector +
      "{" + rs.beginBlock + r.styles.foldLeft("")(_ + render(_, context)) + rs.endBlock + "}" +
    rs.afterRule
  }

  private def render(s: Style, context: RenderContext): String = {
    val rs = context.renderSettings
    rs.beforeProperty + rs.propertyIndent + s.property + rs.beforePropertyColon + ":" +
    rs.afterPropertyColon + context.resolveValue(s.value) + ";" + rs.afterProperty
  }

}