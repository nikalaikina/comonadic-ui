name := "cli-game-lib"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.0")
