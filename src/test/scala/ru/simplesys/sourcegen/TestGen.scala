package ru.simplesys
package sourcegen
package test

import org.scalatest.FunSuite
import sbt._
import ru.simplesys.plugins.sourcegen.meta.{ForeignKeyConstraintType, PK}

//import com.simplesys.log.Logging
//import ru.simplesys.sourcegen.Generator

/*
protected final class Log extends Logger {
  def trace(t: => Throwable) {
    println("Trace: %s", t.toString)
  }

  def success(message: => String) {
    println("message: %s", message)
  }

  def log(level: Level.Value, message: => String) {
    println("message: %s", message)
  }
}
*/

class TestGen extends FunSuite /*with Logging*/ {

/*
  def genSources(sourceDir: File, outputDir: File, pkgName: String, outputCreateScriptsDir: File, logger: Logger): Seq[File] = {
    if (!outputDir.exists) outputDir.mkdirs

    outputCreateScriptsDir.delete
    outputCreateScriptsDir.mkdirs

    logger.log(Level.Info, "before Generator, dir is " + sourceDir.toString() + ", package name is: " + pkgName + ", schema create scripts: " + outputCreateScriptsDir.toString)
    implicit val log = logger

    Generator.processXml((sourceDir ** "*.xml").get, outputDir, pkgName, outputCreateScriptsDir)
  }
*/

/*
  test("generation") {
    val sourceDir = new File("/home/andrew/JOB/fsm-generation/src/main/resources/defs/")
    val outputDir = new File("/home/andrew/JOB/fsm-generation/source-gen/src/test/scala/out")
    val pkgName = "ru.simplesys.defs"
    val outputCreateScriptsDir = new File("/home/andrew/JOB/fsm-generation/source-gen/src/test/scala/out")

    genSources(sourceDir, outputDir, pkgName, outputCreateScriptsDir, new Log)
  }
*/

  test("simple test") {
    ForeignKeyConstraintType.constraintTypes.values.foreach(println)
  }
}