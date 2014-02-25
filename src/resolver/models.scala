package smack.resolver

import smack.parser._

case class CssRule(selector: String, properties: Map[String, String])
case class ModuleTreeNode(val path: String, var children: Map[String, ModuleTreeNode], var module: Option[Module])

object ModuleTreeNode {
  def apply(root: String, parent: Option[ModuleTreeNode], m: Option[Module]) = {
    new ModuleTreeNode(namespaceName(parent, root), Map[String, ModuleTreeNode](), m)
  }

  private def namespaceName(parent: Option[ModuleTreeNode], child: String) = parent match {
    case Some(p) => p.path + "." + child
    case _       => child
  }
}
