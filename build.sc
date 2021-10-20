import mill._, scalalib._

object mhpf extends ScalaModule with PublishModule {
  def scalaVersion = "3.0.2"

  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.2.10"
    )
  }
}
