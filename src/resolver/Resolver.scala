package smack.resolver

import smack.parser._

object Resolver {
  def resolve(smack: SmackFile): List[CssRule] = {
    val moduleTree = modulesToTree(smack.modules)
  }

  private def modulesToTree(ms: List[Module]) = {

  }
}