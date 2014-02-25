package smack.parser

//import scala.collection.mutable.{Map => MutableMap}

class SmackFile(val modules: List[Module]) {
  val moduleTree = namespaceModules(modules, Map[String, ModuleTreeNode]())

  def getModule(path: String): Option[Module] = getNode(path, moduleTree) match {
    case Some(node) => node.module
    case None       => None
  }

  private def namespaceModules(ms: List[Module], tree: Map[String, ModuleTreeNode]) = {
    ms.foldLeft(tree) { (tree, m) =>
      getNode(m.lineage, tree) match {
        case Some(node) => node.module = Some(m); tree
        case None       => tree ++ createNode(m.lineage, None, tree, Some(m))
      }
    }
  }

  private def createNode(path: String, parent: Option[ModuleTreeNode], tree: Map[String, ModuleTreeNode], module: Option[Module] = None): Map[String, ModuleTreeNode] = {
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

  private def getNode(path: String, tree: Map[String, ModuleTreeNode]): Option[ModuleTreeNode] = {
    val segs = namespaceSplit(path)
    if (segs.size > 1) {
      if (tree.contains(segs.head)) getNode(segs.tail.mkString("."), tree.get(segs.head).get.children.toMap)
      else None
    }
    else {
      if (tree.contains(path)) Some(tree.get(path).get)
      else None
    }
  }

  def nsName(parent: Option[ModuleTreeNode], child: String) = parent match {
    case Some(p) => p.path + "." + child
    case _       => child
  }

  private def namespaceSplit(s: String) = {
    s.split("""\.""") match {
      case l if l.size == 0 => Array(s)
      case l                => l
    }
  }
}
