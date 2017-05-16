package ru.simplesys.plugins.sourcegen.app

import ru.simplesys.plugins.sourcegen.meta._
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.scalaGen._
import com.simplesys.scalaGen.ScalaApplyObject
import com.simplesys.scalaGen.ScalaClassParametr
import com.simplesys.scalaGen.ScalaBody
import com.simplesys.scalaGen.ScalaVariable
import com.simplesys.common.Strings._
import ru.simplesys.plugins.sourcegen.meta.LinkToColumnWValue
import com.simplesys.log.Logging

package object Gen extends Logging {
    /*implicit class ColumnDefOpt(column: ColumnDef[_]) {
        def columnType = {
            val _columnType = column.dataType.scalaTypeAsString(column.)
            val columnType = s"${if (column.isMandatory) _columnType else s"Array[${_columnType}]"}"
        }
    }*/

    implicit class Imp(string: String) {
        def tbl = s"${string.capitalize}Tbl"
        def bo = s"${string.capitalize}Bo"
        def ds = s"${string.capitalize}DS"
        def enum = s"${string.capitalize}Enum"
    }

    implicit class Imp1(attr: AttrDef[_])(implicit discriminatorSeq: Seq[LinkToColumnWValue], schemaDef: SchemaDef) {
        def isDiscriminator: Boolean = {
            discriminatorSeq.exists(_.colRef.toCol.scalaName === attr.name)
        }
    }

    implicit class Impl2(serrializeClass: ScalaClassDeclare)(implicit val schema: SchemaDef) {
        def getConstraints(clazz: IClass, addClassName: Boolean = false) {
            val constraints = ScalaSequense(seqName = "Seq", clazz.ucs.map {
                uc =>
                    ScalaApplyObject(
                        name = if (uc.ucType === PK) "PrimaryKey" else "UniqueKey",
                        parametrs = ScalaClassParametrs(
                            ScalaClassParametr(name = "name", `type` = ScalaImplicitType, defaultValue = uc.constrAttrName.dblQuoted),
                            ScalaClassParametr(name = "constraintDate", `type` = ScalaImplicitType, defaultValue = s"${uc.attrNames.map(f => s"KeyConstraintData(${f + (if (addClassName) clazz.className else strEmpty)})").mkString(",")}")
                        )
                    )
            }: _*)

            clazz.fks foreach {
                fk =>
                    constraints += ScalaApplyObject(
                        name = "ForignKey",
                        parametrs = ScalaClassParametrs(
                            ScalaClassParametr(name = "name", `type` = ScalaImplicitType, defaultValue = fk.constrAttrName.dblQuoted),
                            ScalaClassParametr(name = "constraintDate", `type` = ScalaImplicitType, defaultValue = s"${fk.attrNames.map(f => s"ForignKeyConstraintData(${f + (if (addClassName) clazz.className else strEmpty)}, ReferencedData(${fk.referencedClassRef.groupName.dblQuoted},${fk.referencedClassRef.objectName.dblQuoted}), ${fk.isMandatory.toString})").mkString(",")}")
                        )
                    )
            }

            serrializeClass addMember ScalaVariable(name = "constraints", body = ScalaBody(constraints))
        }

        def getConstraints(classes: Seq[(IClass, String)]) {
            val constraints = ScalaSequense(seqName = "Seq");

            classes.zipWithIndex foreach {
                case ((clazz, softNameForCompare), index) =>
                    val suffix = if (softNameForCompare.isEmpty) strEmpty else "_" + softNameForCompare
                    clazz.ucs.map {
                        uc =>
                            constraints += ScalaApplyObject(
                                name = if (uc.ucType === PK) (if (index === 0) "PrimaryKey" else "MigratedPrimaryKey") else "UniqueKey",
                                parametrs = ScalaClassParametrs(
                                    ScalaClassParametr(name = "name", `type` = ScalaImplicitType, defaultValue = (uc.constrAttrName + suffix).dblQuoted),
                                    ScalaClassParametr(name = "constraintDate", `type` = ScalaImplicitType, defaultValue = s"${uc.attrNames.map(f => s"KeyConstraintData(${f + clazz.className.capitalize}${suffix})").mkString(",")}")
                                )
                            )
                    }

                    clazz.fks foreach {
                        fk =>
                            constraints += ScalaApplyObject(
                                name = "ForignKey",
                                parametrs = ScalaClassParametrs(
                                    ScalaClassParametr(name = "name", `type` = ScalaImplicitType, defaultValue = (fk.constrAttrName + suffix).dblQuoted),
                                    ScalaClassParametr(name = "constraintDate", `type` = ScalaImplicitType, defaultValue = s"${fk.attrNames.map(f => s"ForignKeyConstraintData(${f + clazz.className.capitalize}${suffix}, ReferencedData(${fk.referencedClassRef.groupName.dblQuoted},${fk.referencedClassRef.objectName.dblQuoted}), ${fk.isMandatory.toString})").mkString(",")}")
                                )
                            )
                    }
            }

            serrializeClass addMember ScalaVariable(name = "constraints", body = ScalaBody(constraints))
        }
    }
}
