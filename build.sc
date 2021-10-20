// This is for VSCode/Metals cupport => After mill, also run this: mill mill.contrib.Bloop/install
import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`

import mill._, scalalib._

object mhpf extends ScalaModule {
  def scalaVersion = "3.0.2"

  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.2.10"
    )
  }
}
