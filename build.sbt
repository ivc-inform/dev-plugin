sbtPlugin := true

name := "dev-plugin"

enablePlugins(GitVersioning)

organization := "ru.simplesys"

//version := "1.0.10-SNAPSHOT"

scalaVersion := "2.10.6"

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
	val ssysCoreVersion = "1.2.10"
	//val ssysCoreVersion = "1.2-SNAPSHOT"
	val scalazVersion = "7.2.4"
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
        "org.postgresql" % "postgresql" % "9.2-1003-jdbc4" % "test",
        "org.specs2" %% "specs2" % "2.4.2" % "test",
        "org.mockito" % "mockito-all" % "1.9.5" % "test",
        "org.scalatest" %% "scalatest" % "2.2.1" % "test"
    )
}

logLevel := Level.Info

git.baseVersion := "1.0.11"



