scalaVersion := "2.11.11"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.davegurnell" %% "checklist" % "0.2.0",
  "com.github.nscala-time" %% "nscala-time" % "2.16.0",
  "io.buildo" %% "enumero" % "1.2.1"
)

resolvers ++= Seq(
  Resolver.bintrayIvyRepo("scalameta", "maven"),
  Resolver.bintrayRepo("buildo", "maven")
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
