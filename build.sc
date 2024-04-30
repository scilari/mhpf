import mill._, scalalib._

object mhpf extends ScalaModule with ScalaJSModule {
  def scalaVersion = "3.3.3"
  def scalaJSVersion = T { "1.16.0" }

  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-parallel-collections::1.0.4"
  )

  object test extends ScalaTests with TestModule.ScalaTest {
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.2.10",
      ivy"org.scala-lang.modules::scala-parallel-collections::1.0.4"
    )
  }
}
