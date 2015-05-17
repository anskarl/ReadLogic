name := "ReadLogic"

version := "0.1"

organization := "com.github.anskarl"

scalaVersion := "2.11.6"

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
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

// Logging with slf4j and logback
libraryDependencies ++= Seq(
	"ch.qos.logback" % "logback-classic" % "1.1.2",
	"ch.qos.logback" % "logback-core" % "1.1.2",
	"org.slf4j" % "slf4j-api" % "1.7.10"
)

// Unit testing
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"