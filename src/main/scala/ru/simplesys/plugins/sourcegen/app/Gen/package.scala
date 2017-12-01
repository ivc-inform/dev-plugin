package ru.simplesys.plugins.sourcegen.app

import com.simplesys.common.Strings._
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.log.Logging
import com.simplesys.scalaGen.{ScalaApplyObject, ScalaBody, ScalaClassParametr, ScalaVariable, _}
import ru.simplesys.plugins.sourcegen.meta.{LinkToColumnWValue, _}

package object Gen extends Logging {
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
        def getConstraints(clazz: IClass, addClassName: Boolean = false, forLob: Boolean = false) {
            val constraints =
                if (!forLob)
                    ScalaSequense(seqName = "Seq", clazz.ucs.map {
                        uc =>
                            ScalaApplyObject(
                                name = if (uc.ucType === PK) "PrimaryKey" else "UniqueKey",
                                parametrs = ScalaClassParametrs(
                                    ScalaClassParametr(name = "name", `type` = ScalaImplicitType, defaultValue = {
                                        import com.simplesys.common.JVM.Strings._
                                        uc.constrAttrName.dblQuoted
                                    }),
                                    ScalaClassParametr(name = "constraintDate", `type` = ScalaImplicitType, defaultValue = s"${uc.attrNames.map(f => s"KeyConstraintData(${f + (if (addClassName) clazz.className else strEmpty)})").mkString(",")}")
                                )
                            )
                    }: _*)
                else
                    ScalaSequense(seqName = "Seq", Seq(clazz.pk).map {
                        uc =>
                            ScalaApplyObject(
                                name = if (uc.ucType === PK) "PrimaryKey" else "UniqueKey",
                                parametrs = ScalaClassParametrs(
                                    ScalaClassParametr(name = "name", `type` = ScalaImplicitType, defaultValue = {
                                        import com.simplesys.common.JVM.Strings._
                                        uc.constrAttrName.dblQuoted
                                    }),
                                    ScalaClassParametr(name = "constraintDate", `type` = ScalaImplicitType, defaultValue = s"${uc.attrNames.map(f => s"KeyConstraintData(${f + (if (addClassName) clazz.className else strEmpty)})").mkString(",")}")
                                )
                            )
                    }: _*)

            if (!forLob)
                clazz.fks foreach {
                    fk =>
                        constraints += ScalaApplyObject(
                            name = "ForignKey",
                            parametrs = ScalaClassParametrs(
                                ScalaClassParametr(name = "name", `type` = ScalaImplicitType, defaultValue = {
                                    import com.simplesys.common.JVM.Strings._
                                    fk.constrAttrName.dblQuoted
                                }),
                                ScalaClassParametr(name = "constraintDate", `type` = ScalaImplicitType, defaultValue = s"${fk.attrNames.map(f => s"ForignKeyConstraintData(${f + (if (addClassName) clazz.className else strEmpty)}, ReferencedData(${
                                    {
                                        import com.simplesys.common.JVM.Strings._
                                        fk.referencedClassRef.groupName.dblQuoted
                                    }},${
                                    {
                                        import com.simplesys.common.JVM.Strings._
                                        fk.referencedClassRef.objectName.dblQuoted
                                    }}), ${fk.isMandatory.toString})").mkString(",")}")
                            )
                        )
                }

            serrializeClass addMember ScalaVariable(name = "constraints", body = ScalaBody(constraints))
        }

        def getConstraints(classes: Seq[(IClass, String)], forLob: Boolean) {
            val constraints = ScalaSequense(seqName = "Seq");

            classes.zipWithIndex foreach {
                case ((clazz, softNameForCompare), index) =>
                    val suffix = if (softNameForCompare.isEmpty) strEmpty else "_" + softNameForCompare
                    if (!forLob)
                        clazz.ucs.map {
                            uc =>
                                constraints += ScalaApplyObject(
                                    name = if (uc.ucType === PK) (if (index === 0) "PrimaryKey" else "MigratedPrimaryKey") else "UniqueKey",
                                    parametrs = ScalaClassParametrs(
                                        ScalaClassParametr(name = "name", `type` = ScalaImplicitType, defaultValue = {
                                            import com.simplesys.common.JVM.Strings._
                                            (uc.constrAttrName + suffix).dblQuoted
                                        }),
                                        ScalaClassParametr(name = "constraintDate", `type` = ScalaImplicitType, defaultValue = s"${uc.attrNames.map(f => s"KeyConstraintData(${f + clazz.className.capitalize}${suffix})").mkString(",")}")
                                    )
                                )
                        }
                    else
                        Seq(clazz.pk).map {
                            uc =>
                                constraints += ScalaApplyObject(
                                    name = if (uc.ucType === PK) (if (index === 0) "PrimaryKey" else "MigratedPrimaryKey") else "UniqueKey",
                                    parametrs = ScalaClassParametrs(
                                        ScalaClassParametr(name = "name", `type` = ScalaImplicitType, defaultValue = {
                                            import com.simplesys.common.JVM.Strings._
                                            (uc.constrAttrName + suffix).dblQuoted
                                        }),
                                        ScalaClassParametr(name = "constraintDate", `type` = ScalaImplicitType, defaultValue = s"${uc.attrNames.map(f => s"KeyConstraintData(${f + clazz.className.capitalize}${suffix})").mkString(",")}")
                                    )
                                )
                        }

                    if (!forLob)
                        clazz.fks foreach {
                            fk =>
                                constraints += ScalaApplyObject(
                                    name = "ForignKey",
                                    parametrs = ScalaClassParametrs(
                                        ScalaClassParametr(name = "name", `type` = ScalaImplicitType, defaultValue = {
                                            import com.simplesys.common.JVM.Strings._
                                            (fk.constrAttrName + suffix).dblQuoted
                                        }),
                                        ScalaClassParametr(name = "constraintDate", `type` = ScalaImplicitType, defaultValue = s"${fk.attrNames.map(f => s"ForignKeyConstraintData(${f + clazz.className.capitalize}${suffix}, ReferencedData(${
                                            {
                                                import com.simplesys.common.JVM.Strings._
                                                fk.referencedClassRef.groupName.dblQuoted
                                            }},${
                                            {
                                                import com.simplesys.common.JVM.Strings._
                                                fk.referencedClassRef.objectName.dblQuoted
                                            }}), ${fk.isMandatory.toString})").mkString(",")}")
                                    )
                                )
                        }
            }

            serrializeClass addMember ScalaVariable(name = "constraints", body = ScalaBody(constraints))
        }
    }
}
