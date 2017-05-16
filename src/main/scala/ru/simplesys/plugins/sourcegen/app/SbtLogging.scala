package com.simplesys.log

import sbt.Level
import org.slf4j.{Logger => Slf4jLogger, LoggerFactory => Slf4jLoggerFactory}

object SbtLogging {
    def apply(_class: Class[_]): Logger = new BasicLogger(Slf4jLoggerFactory.getLogger(_class))
}

trait SbtLogging extends sbt.Logger {
    private lazy val logger = Slf4jLoggerFactory.getLogger(this getClass)

    def trace(t: => Throwable): Unit = logger.trace("", t)
    def success(message: => String): Unit = logger trace message

    def log(level: Level.Value, message: => String) {
        level match {
            case Level.Debug => logger debug message
            case Level.Error => logger error message
            case Level.Info => logger info message
            case Level.Warn => logger warn message
            case _ => logger trace message
        }
    }
}