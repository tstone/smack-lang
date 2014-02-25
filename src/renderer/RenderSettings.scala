package smack.renderer

case class RenderSettings(
  beginBlock: String = "\n",
  endBlock: String = "",

  beforeModule: String = "",
  afterModule: String = "\n",
  afterModuleSelector: String = " ",

  beforeRule: String = "",
  afterRule: String = "\n\n",
  afterRuleSelector: String = " ",

  propertyIndent: String = "  ",
  beforeProperty: String = "",
  beforePropertyColon: String = "",
  afterPropertyColon: String = " ",
  afterProperty: String = "\n"
)