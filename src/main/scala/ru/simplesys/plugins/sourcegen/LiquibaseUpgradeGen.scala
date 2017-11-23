package ru.simplesys.plugins
package sourcegen

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import liquibase.Liquibase
import liquibase.diff.output.DiffOutputControl
import liquibase.integration.commandline.CommandLineUtils
import liquibase.resource.FileSystemResourceAccessor
import sbt._

import scala.xml.{Null, UnprefixedAttribute, XML}

object LiquibaseUpgradeGen {
  import XmlUtil._

  def newEmptyUpgradeChangeLog = {
    <databaseChangeLog xmlns="http://www.liquibase.org/xml/ns/dbchangelog"
                       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                       xsi:schemaLocation="http://www.liquibase.org/xml/ns/dbchangelog http://www.liquibase.org/xml/ns/dbchangelog/dbchangelog-3.0.xsd" objectQuotingStrategy="QUOTE_ALL_OBJECTS">
    </databaseChangeLog>
  }

  private def setObjectQuotingStrategy(changelog: File) = {

    val clog = XML.load(new java.io.InputStreamReader(new java.io.FileInputStream(changelog), XmlUtil.Encoding))
    // val clog = XML.loadFile(changelog)
    val clogMod = clog % new UnprefixedAttribute("objectQuotingStrategy", "QUOTE_ALL_OBJECTS", Null)
//    val clogMod = clog
    save(clogMod, changelog)
  }

  private def addChangeLogToMaster(master: File, changelog: File, baseDir: File) = {
    val upgradeLog = XML.loadFile(master)
    val toAdd = <include file={changelog.relativeTo(baseDir).get.getPath}/>
    val newLog = addChild(upgradeLog, toAdd)
    save(newLog, master)
    setObjectQuotingStrategy(changelog)
  }

  def generateUpgradeChangelog1(upgradeDir: File, createChLog: File, upgradeChLog: File, baseDir: File)(implicit log: Logger): Unit = {
    log.info("entered generateUpgradeChangelog")
  }

  def generateUpgradeChangelog(upgradeDir: File, createChLog: File, upgradeChLog: File, baseDir: File)(implicit log: Logger): Unit = {
    log.info("entered generateUpgradeChangelog")
    upgradeDir.mkdirs()

    if (!createChLog.exists()) {
      val err = "File " + createChLog.getPath + " doesn't exists!"
      log.log(Level.Error, err)
      throw new RuntimeException(err)
    } else {

      if (!upgradeChLog.exists()) {
        log.log(Level.Info, "Upgrade changelog doesn't exist, using create changelog as start point")
        save(newEmptyUpgradeChangeLog, upgradeChLog)

        val username = sys.env.get("USER").getOrElse(sys.env("USERNAME"))
        val datetime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("YYYY.MM.dd-HH_mm_ss"))

        val initial = upgradeDir / ("db.initial-" + datetime + "-" + username + ".xml")
        initial.delete()
        IO.copyFile(createChLog, initial)
        addChangeLogToMaster(upgradeChLog, initial, baseDir)

      }
      else {
        //val driver = "org.hsqldb.jdbc.JDBCDriver"
        val driver = "org.h2.Driver"
        //jdbc:h2:mem:create;MODE=Oracle
        //jdbc:hsqldb:mem:create;shutdown=true

        //        liquibase.datatype.DataTypeFactory.getInstance().register(classOf[H2ModDateTimeType])
        //        liquibase.datatype.DataTypeFactory.getInstance().register(classOf[H2ModNumericType])

        val dbCreate = CommandLineUtils.createDatabaseObject(this.getClass.getClassLoader, "jdbc:h2:mem:create;MODE=Oracle", "test", "", driver, null, null, null, null)
        val liquiCreate = new Liquibase(createChLog.getPath, new FileSystemResourceAccessor, dbCreate)
        val dbUpgrade = CommandLineUtils.createDatabaseObject(this.getClass.getClassLoader, "jdbc:h2:mem:upgrade;MODE=Oracle", "test", "", driver, null, null, null, null)
        val liquiUpgrade = new Liquibase(upgradeChLog.getPath, new FileSystemResourceAccessor, dbUpgrade)

        try {
          log.log(Level.Info, "Creating and populating DB with creation changelog...")
          liquiCreate.update("")
          log.log(Level.Info, "Creating and populating DB with upgrade changelog...")
          liquiUpgrade.update("")

          val username = sys.env.get("USER").getOrElse(sys.env("USERNAME"))
          val datetime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("YYYY.MM.dd-HH_mm_ss"))

          val upgrade = upgradeDir / ("db.upgrade-" + datetime + "-" + username + ".xml")

          log.log(Level.Info, "Generating diff between DBs...")
          CommandLineUtils.doDiffToChangeLog(upgrade.getPath, dbCreate, dbUpgrade, new DiffOutputControl(false, false, false))

          val changeset = XML.load(upgrade.getPath)
          if (changeset.child.isEmpty) IO.delete(upgrade)
          else addChangeLogToMaster(upgradeChLog, upgrade, baseDir)
        }
        finally {
          dbCreate.close()
          dbUpgrade.close()
        }
      }

    }
  }

}
