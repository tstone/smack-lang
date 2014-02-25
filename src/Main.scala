import smack.parser._
import smack.renderer._
import smack.linter._

object App {
  def main(args: Array[String]) = {
    val code =
      """

      @module ident-123 >> name.whatever {
        @color = #fff
        @font-size = 12px;

        ul { color: steelblue; }

        li {
          font-size: @font-size
          color: @ul.color
          color: @div.color
        }

        div{color:@color;}
      }

      """.stripMargin

    val smack = Parser(code)

    Linter.lint(smack).foreach(println)
    println
    println(Renderer.render(smack))

  }
}
