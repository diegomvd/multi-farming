val scala3Version = "3.1.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "build-test",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      //"org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.scala-lang" %% "scala3-library" % "3.1.2",
      "org.apache.spark" % "spark-core_2.13" % "3.3.0",
      "org.apache.spark" % "spark-graphx_2.12" % "3.3.0",
      "org.scala-graph" %% "graph-core" % "1.13.5"
    )
  )
