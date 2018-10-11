name := "mhpf"

organization := "com.scilari"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

publishMavenStyle := true

crossScalaVersions := Seq("2.11.8", "2.12.4")

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

javacOptions ++= Seq("-source", "1.7", "-target", "1.7")
scalacOptions += "-target:jvm-1.7"

publishArtifact in Test := false

parallelExecution in Test := false

pomIncludeRepository := { _ => false }

pomExtra :=
  <url>https://github.com/scilari/mhpf</url>
    <licenses>
      <license>
        <name>MIT</name>
        <url>https://opensource.org/licenses/MIT</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <developers>
      <developer>
        <id>scilari</id>
        <name>Ilari Vallivaara</name>
        <url>https://scilari.com/</url>
      </developer>
    </developers>

        