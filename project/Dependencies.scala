import sbt._

object Dependencies {

  val runtimeDependencies: List[ModuleID] =
    "com.slamdata" %% "matryoshka-core" % "0.21.2" ::
    "com.lihaoyi"  %% "fastparse"       % "1.0.0"  ::
    Nil

  val testDependencies: List[ModuleID] =
    "org.scalatest" %% "scalatest" % "3.0.4" ::
    Nil
}
