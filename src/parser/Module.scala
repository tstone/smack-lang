package smack.parser

case class Module(name: String, parent: Option[String], body: ModuleBlock) {

//  protected val ruleMap: Map[String, Rule] = mapRules(body.rules)

  val lineage = parent match {
    case Some(parent) => parent + "." + name
    case None         => name
  }



  private def mapRules(rules: List[Rule]) = {

  }

}
