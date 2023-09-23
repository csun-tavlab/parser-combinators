name := "nondet"

scalaVersion := "2.13.8"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.17"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test"
