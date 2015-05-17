name := "ReadLogic"

version := "0.1"

organization := "com.github.anskarl"

scalaVersion := "2.11.6"

autoScalaLibrary := true

managedScalaInstance := true

// fork a new JVM for 'run' and 'test:run'
fork := true

// fork a new JVM for 'test:run', but not 'run'
fork in Test := true

/** Dependencies */
resolvers ++= Seq(
	"typesafe" at "http://repo.typesafe.com/typesafe/releases/",
	"sonatype-oss-public" at "https://oss.sonatype.org/content/groups/public/")
	
// Scala-lang
libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % scalaVersion.value,
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

// Scala-modules
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

// Unit testing
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"



val publishSettings: Setting[_] = publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))