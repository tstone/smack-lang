package smack.resolver

import smack.parser._

class ModuleTree(ms: List[Module]) {
  private val tree = treeifyModules(ms)
  
  def get(path: String) = node(path, tree)

  private def treeifyModules(ms: List[Module]) = {
    ms.foldLeft(Map[String, ModuleTreeNode]()) { tree, m =>
      node(m.lineage, tree) match {
        case Some(node) => node.module = Some(m); tree
        case None       => tree ++ createNode(m.lineage, None, tree, Some(m))
      }
    }
  }

  private def subTree(root: String, tree: Map[String, ModuleTreeNode]) = tree.get(root).get.children

  private def node(path: String, tree: Map[String, ModuleTreeNode]): Option[ModuleTreeNode] = {
    MId(path) match {
      case MId(root, None)    => tree.get(root)
      case MId(root, Some(t)) => if (tree.contains(root)) node(t, subTree(root, tree)) else None
      case _                  => None
    }
  }

  private def createNode(path: String, parent: Option[ModuleTreeNode], tree: Map[String, ModuleTreeNode], module: Option[Module] = None): Map[String, ModuleTreeNode] = {
    val id = MId(path)
    (id.tail, node(id.root, tree))match {
      case (Some(rightPath), Some(node)) => createNode(rightPath, Some(node), subTree(id.root, tree), module)
      case (Some(rightPath), None)       => createParentNode(id, parent, tree, module)
      case (None, None)                  =>
    }


//    val id = MId(path)
//    node(id.root, tree) match {
//      case node: Some => createNode(id.tail, node, subTree(id.root, tree), module)
//      case MId(root, Some(t)) => createNode(t, Some(node), subTree(root, tree), module)
//      case MId(root, None)    => tree.get(root)
//    }

    val segs = namespaceSplit(path)
    getNode(segs(0), tree) match {
      case Some(node) => createNode(segs.tail.mkString("."), Some(node), tree.get(segs.head).get.children, module)
      case None       => {
        var nodeModule = if (segs.size > 1) None else module
        val newNode = ModuleTreeNode(nsName(parent, segs.head), Map[String, ModuleTreeNode](), nodeModule)
        val newTree = tree ++ Map(segs.head -> newNode)
        if (segs.size > 1) newTree ++ createNode(segs.tail.mkString("."), Some(newNode), newNode.children, module)
        else newTree
      }
    }
  }

  private def createParentNode(id: MId, parent: Option[ModuleTreeNode], tree: Map[String, ModuleTreeNode], module: Option[Module] = None) = {
    val parent = ModuleTreeNode(id.root, parent, None)
    tree ++ Map(id.root -> parent) ++ createNode()
  }

  // Type to describe a module's path
  private case class MId(root: String, tail: Option[String])

  object MId {
    def apply(path: String) = {
      path.split("""\.""") match {
        case l if l.size == 0 => MId(path, None)
        case l                => MId(l.head, Some(l.tail.mkString(".")))
      }
    }
  }
}