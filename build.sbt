import sbtcrossproject.CrossPlugin.autoImport.crossProject

ThisBuild / scalaVersion := "2.13.5"

lazy val root = project.in(file(".")).
  aggregate(proj.js, proj.jvm).
  settings(
    publish := {},
    publishLocal := {},
  )

lazy val proj = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  settings(
    name := "natural-deduction",
    version := "0.1",
    scalacOptions := Seq("-unchecked", "-deprecation"),
    libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2",
    libraryDependencies += "com.lihaoyi" %%% "upickle" % "1.3.8",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.1.1" % "test",
  ).
  jvmSettings().
  jsConfigure(_.enablePlugins(ScalaJSBundlerPlugin)).
  jsSettings(
    npmDependencies in Compile ++= Seq(
      "react" -> "17.0.2",
      "react-dom" -> "17.0.2"),
    scalaJSUseMainModuleInitializer := true,
    mainClass in Compile := Some("Main"),
    scalacOptions := Seq("-unchecked", "-deprecation"),
    libraryDependencies += "com.github.japgolly.scalajs-react" %%% "core" % "1.7.7",
    libraryDependencies += "com.github.japgolly.scalajs-react" %%% "extra" % "1.7.7"
  )
