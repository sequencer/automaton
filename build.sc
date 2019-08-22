import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalalib.scalafmt._

object automaton extends ScalaModule with PublishModule with ScalafmtModule {
  def scalaVersion = "2.12.8"

  override def ivyDeps = Agg(
    ivy"edu.berkeley.cs::firrtl:1.2-SNAPSHOT",
    ivy"edu.berkeley.cs::chisel3:3.2-SNAPSHOT",
    ivy"com.lihaoyi::upickle:0.7.5"
  )

  def publishVersion = "0.0.1"

  def pomSettings = PomSettings(
    description = "automaton",
    organization = "me.sequencer",
    url = "https://github.com/sequencer/automaton",
    licenses = Seq(License.`BSD-3-Clause`),
    versionControl = VersionControl.github("sequencer", "automaton"),
    developers = Seq(
      Developer("sequencer", "Jiuyang Liu", "https://github.com/sequencer")
    )
  )

  object tests extends Tests {
    override def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.1")

    def testFrameworks = Seq("utest.runner.Framework")
  }

}
