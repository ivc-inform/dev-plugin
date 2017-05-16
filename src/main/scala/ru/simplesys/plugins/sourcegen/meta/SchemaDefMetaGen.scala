package ru.simplesys.plugins
package sourcegen
package meta

import sbt._

//import ru.simplesys.meta.types.StringAddons
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.genSources._
import com.simplesys.common.Strings._

trait SchemaDefMetaGen {
    self: SchemaDef =>

    def generateClasses(group: IGroup, startPackageName: String): String = {
        val out = new StringBuilder()

        val groupName = group.name

        out append genMessageCreating("SchemaDefMetaGen (generateClasses)").newLine
        out append s"package ${startPackageName}".newLine
        out append s"package ${groupName}".newLine

        val importsClasses =
          """
            |
            |import org.joda.time.{DateTime, LocalDateTime}
            |import ru.simplesys.meta.types.{MetaType, Domain}
            |import ru.simplesys.meta._
            |import ru.simplesys.coreutil.{SealedEnumRuntime, SealedCaseClassEnum}
            |import com.simplesys.jdbc._
            |import com.simplesys.json.JsonString
            |import com.simplesys.corelibrary.domain._
            |import com.simplesys.common.array._
            |import com.simplesys.jdbc.control.clob._
            |
            | """.stripMargin

        out append importsClasses

        val localClasses = classes.filter(_.group === group.selfRef)

        val enumsString = localClasses.map(_.genObjectsForClass(this)).filterNot(_.length === 0).mkString(newLine.newLine)

        out append enumsString
        out append newLine.newLine

        val classDefsString = localClasses.map(_.genClassDefs(this)).filterNot(_.length === 0).mkString(newLine.newLine)

        out append classDefsString

        out.toString().chmp
    }

    def generateTables(group: IGroup, startPackageName: String): String = {
        val out = new StringBuilder()

        val groupName = group.name

        out append genMessageCreating("SchemaDefMetaGen (generateTables)").newLine
        out append s"package ${startPackageName}".newLine
        out append s"package ${groupName}".newLine

        val importsTables =
            """
              |
              |import org.joda.time.{DateTime, LocalDateTime}
              | """.stripMargin

        out append importsTables
        /*

            val localEnumClasses = enumClasses.filter(_.group === group.selfRef)
            val implicitMappersJOOQ = localEnumClasses.map(x => s"  implicit class ${x.objJOOQMapperName}(dt: DataType[${x.keyMemberType.simpleScalaType}]  ) {def ${x.funcJOOQMapperName} = dt.asConvertedDataType(${x.className})}")


            val localClasses = classes.filter(cl => cl.group === group.selfRef)
            //val localClassesWSimpleEnums = localClasses.exists(_.strictAttrs.exists)
            val implicitMappersForSimpleEnumsJOOQ = localClasses.flatMap(x => x.strictAttrs.collect{case x: EnumAttrDef[_] => x}.map(attr =>
              s"  implicit class ${attr.objJOOQMapperName} (dt: DataType[${attr.keyMemberType.simpleScalaType}]) {def ${attr.funcJOOQMapperName} = dt.asConvertedDataType(${attr.objName})}"
            ))



            val mapperObjectJOOQ = s"""|object mappersJOOQ {
                                       |${implicitMappersJOOQ.mkString("\n")}
                                       |${implicitMappersForSimpleEnumsJOOQ.mkString("\n")}
                                       |}
                                       |
                                       |""".stripMargin

            out append mapperObjectJOOQ

        */

        val localTables = tables.filter(_.group === group.selfRef)
        val tablesDefString = localTables.map(_.genTableObjectJOOQ(this)).filterNot(_.length === 0).mkString(newLine.newLine)

        out append tablesDefString

        out.toString().chmp
    }

    def generateMetadata(group: IGroup, startPackageName: String): String = {
        val out = new StringBuilder()

        val groupName = group.name

        out append genMessageCreating("SchemaDefMetaGen (generateMetadata)").newLine
        out append s"package ${startPackageName}".newLine
        out append s"package ${groupName}".newLine

        val importsMetas =
            """
              |
              |import shapeless._
              |import ops.hlist._
              |
              |import org.joda.time.{DateTime, LocalDateTime}
              |
              |import scalaz.syntax.std.option._
              |import scalaz.std.option._
              |import scalaz.std.string._
              |
              |import ru.simplesys.meta.types.{MetaType, Domain}
              |import ru.simplesys.meta._
              |import ru.simplesys.coreutil.{SealedEnumRuntime, SealedCaseClassEnum}
              |import com.simplesys.json.JsonString
              |
              | """.stripMargin

        out append importsMetas

        val localClasses = classes.filter(_.group === group.selfRef)

        val metaDefs = localClasses.map(_.genClassMeta(this)).filterNot(_.length === 0).mkString(newLine.newLine)

        out append metaDefs

        out.toString().chmp
    }


    def generateScalaCode(outputScalaCodeDir: File, startPackageName: String)(implicit log: Logger): Seq[File] = {
        log.info("entered generateScalaCode")
        IO.delete(outputScalaCodeDir)
        outputScalaCodeDir.mkdirs()

        val filesBO = groups.map {
            group =>
                val groupName = group.name
                val classesOut = outputScalaCodeDir / s"${groupName.capitalize}Classes.scala"
                /*val tablesOut = outputScalaCodeDir / s"${groupName.capitalize}Tables.scala"
                val metaOut = outputScalaCodeDir / s"${groupName.capitalize}Meta.scala"*/

                val classesString = generateClasses(group, startPackageName)
                IO.write(classesOut, classesString)

                /*val tablesString = generateTables(group, startPackageName)
                IO.write(tablesOut, tablesString)*/

                /*val metadatasString = generateMetadata(group, startPackageName)
                IO.write(metaOut, metadatasString)*/

                Seq(classesOut /*, tablesOut, metaOut*/)
        }.flatten
        filesBO
    }

    def generateJavaScript(outputJavaScriptDir: File)(implicit log: Logger): Seq[File] = {
        log.info("entered generateJavaScript")
        Seq()
    }
}
