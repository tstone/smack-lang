package smack.linter

import smack.parser._

object Linter {

  def lint(smack: SmackFile): List[String] = {
    namespaceWithoutModule(smack.moduleTree)
  }

  private def warn(s: String) = "[WARN] " + s

  private def namespaceWithoutModule(tree: Map[String, ModuleTreeNode]): List[String] = {
    tree.foldLeft(List[String]()) { case (acc, (key, node)) => {
      if (node.module.isEmpty) acc ++ List(warn("Module `" + node.path + "` does not exist."))
      else acc
    }}
  }



}