import mill._, scalalib._

object mhpf extends ScalaModule {
  def scalaVersion = "3.0.2"

  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-parallel-collections::1.0.4"
  )

  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.2.10",
      ivy"org.scala-lang.modules::scala-parallel-collections::1.0.4"
    )
  }
}
