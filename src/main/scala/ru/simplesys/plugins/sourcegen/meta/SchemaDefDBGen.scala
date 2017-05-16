package ru.simplesys.plugins
package sourcegen
package meta

import sbt._

trait SchemaDefDBGen {
  self: SchemaDef =>

  def genCreateScripts(implicit log: Logger) = {
    val baseChangeSetID = org.joda.time.DateTime.now().toString("YYYY.MM.dd")
    <databaseChangeLog xmlns="http://www.liquibase.org/xml/ns/dbchangelog"
                       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                       xsi:schemaLocation="http://www.liquibase.org/xml/ns/dbchangelog
                                           http://www.liquibase.org/xml/ns/dbchangelog/dbchangelog-3.1.xsd" objectQuotingStrategy="QUOTE_ALL_OBJECTS">
      {for (table <- tables.sortBy(t => t.group + t.tableName)) yield table.genCreateScript(baseChangeSetID)(this)}{for (table <- tables.filterNot(_.fks.isEmpty)) yield table.genFKScript(baseChangeSetID)(this)}
    </databaseChangeLog>
  }

  def generateCreateChangelog(/*outputCreateChangelogDir: File, */liquibaseCreateChangelog: File)(implicit log: Logger): Seq[File] = {
    log.info("entered generateCreateChangelog")

    IO.delete(liquibaseCreateChangelog)
    liquibaseCreateChangelog.getParentFile.mkdirs

    XmlUtil.save(genCreateScripts, liquibaseCreateChangelog)
    Seq(liquibaseCreateChangelog)
  }

}
