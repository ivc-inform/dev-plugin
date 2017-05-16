package ru.simplesys.plugins
package sourcegen
package meta


trait TableDefMetaGen {
  self: TableDef =>

  def genTableObjectJOOQ(implicit resolver: SchemaDef): String = {
    val tblName = tableDBName(resolver)

    val recordType = if (columns.size > maxArity) "Record" else s"Record${columns.size.toString}[${columns.map(_.scalaTypeAsString(selfRef.groupName, resolver)).mkString(", ")}]"
    val stringAttrs = columns.map(x => s"""  val ${x.scalaName} = createFieldScala("${x.dbName}", SQLDataType.${x.jooqTypeDefAsString(selfRef.groupName, resolver)}, this)""")


    s"""|class ${scalaTableName} private(nameJOOQTable: String, aliased: Table[${recordType}]) extends AbstractTableImplScala[${recordType}](nameJOOQTable, null, aliased) {
        |  //import mappersJOOQ._
        |  val tableNameDB = "${tblName}"
        |
        |${stringAttrs.mkString("\n")}
        |}
        |
        |object ${scalaTableName} extends ${scalaTableName}("${tblName}", null) {
        |  def apply(alias: String) = new ${scalaTableName}(alias, this)
        |  override def as(alias: String) = apply(alias)
        |}
        |""".stripMargin
  }

}
