name := "scripts"

organization := "jp.ac.keio.flet"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.0"

resolvers += "typesafe" at "http://repo.typesafe.com/typesafe/snapshots"

libraryDependencies ++= Seq(
	"javax.mail" % "mail" % "1.4"
)


//initialCommands := "import com.qualcomm.postgrestest._"
