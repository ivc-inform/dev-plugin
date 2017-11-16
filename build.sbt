sbtPlugin := true

name := "dev-plugin"

//enablePlugins(GitVersioning)

organization := "ru.simplesys"

version := "1.3.14-SNAPSHOT"

scalaVersion := "2.12.4"

scalacOptions := Seq(
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:existentials",
    "-language:reflectiveCalls",
    "-language:postfixOps",
    "-deprecation",
    "-unchecked")


description := "sbt plugin to generate sources"

publishArtifact in(Compile, packageBin) := true

publishArtifact in(Test, packageBin) := false

publishArtifact in(Compile, packageDoc) := false

publishArtifact in(Compile, packageSrc) := true

publishMavenStyle := true

libraryDependencies ++= {
	//val ssysCoreVersion = "1.4.0.2"
	val ssysCoreVersion = "1.4-SNAPSHOT"
	val scalazVersion = "7.2.15"
	val scalaFmtVersion = "1.3.0"
    Seq(
        "com.simplesys.core" %% "core-domains" % ssysCoreVersion,
        "com.simplesys.core" %% "core-utils" % ssysCoreVersion,
        "com.simplesys.core" %% "common" % ssysCoreVersion,
        "com.simplesys.core" %% "xml-extender" % ssysCoreVersion,
        "com.simplesys.core" %% "saxon-wrapper" % ssysCoreVersion,
        "com.simplesys.core" %% "scala-gen" % ssysCoreVersion,
        "org.scalaz" %% "scalaz-core" % scalazVersion,
        //"org.liquibase" % "liquibase-maven-plugin" % "3.5.1",
        "com.h2database" % "h2" % "1.4.192",
        "com.geirsson" %% "scalafmt-core" % scalaFmtVersion,
        "com.geirsson" %% "scalafmt-cli" % scalaFmtVersion,
        "org.postgresql" % "postgresql" % "9.2-1003-jdbc4" % Test,
        "org.mockito" % "mockito-all" % "1.9.5" % Test,
        "org.scalatest" %% "scalatest" % "3.0.4" % Test
    )
}

logLevel := Level.Info

//git.baseVersion := "1.3.11"



