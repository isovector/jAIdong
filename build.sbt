name := """jAIdong"""

version := "1.0"

scalaVersion := "2.11.4"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "shapeless-scalacheck" % "0.3",
  "org.typelevel" %% "shapeless-spire" % "0.3",
  "org.typelevel" %% "shapeless-scalaz" % "0.3"
)

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test"

