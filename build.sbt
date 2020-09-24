name := "retirement-calculator"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.0",
  "org.scalatest" %% "scalatest" % "3.2.0" % "test",
  "org.typelevel" %% "cats-core" % "1.0.1")

mainClass in Compile := Some("retcalc.SimulatePlanApp")
