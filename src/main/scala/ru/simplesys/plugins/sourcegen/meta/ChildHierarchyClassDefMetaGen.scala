package ru.simplesys.plugins
package sourcegen
package meta

//import ru.simplesys.meta.types.StringAddons
import com.simplesys.common.Strings._
import com.simplesys.common.equality.SimpleEquality._
import ru.simplesys.meta.types.{DomainBlob, DomainClob}

trait ChildHierarchyClassDefMetaGen extends AbstractClassDefMetaGen {
    self: ChildHierarchyClassDef =>

    override def genUCRefs(implicit resolver: SchemaDef): Seq[String] = {
        val strictUCRefs = super.genUCRefs

        val inheritedUCRefs = ucs.filter(uc => !strictUCs.exists(_.selfRefByName === uc.selfRefByName)).map { x =>
            val ucAttrs = x.attrs.map { x =>
                s"""${x.name}: ${x.scalaTypeAsString(group, resolver)}"""
            }.mkString(", ")

            val parentUC = parentClassDef.ucs.find(_.softNameForCompare === x.softNameForCompare).get
            val ucAttrsParams = x.attrs.map { x =>
                s"""${x.name}: ${x.scalaTypeAsString(group, resolver)}"""
            }.mkString(", ")

            s"""|class ${x.classRefName}(${ucAttrs}) extends ${parentUC.classRefNameRelative(group)}(${parentUC.attrNames.mkString(", ")}) with BOReference[${className}]
                |
            |object ${x.classRefName} {
                |  def apply(${ucAttrsParams}): ${x.classRefName} = new ${x.classRefName}(${x.attrNames.mkString(", ")})
                |}
                |""".stripMargin

        }

        strictUCRefs ++ inheritedUCRefs
    }

    override def genUCAttrs(implicit resolver: SchemaDef): Seq[String] = {
        if (isAbstract) {
            val attrUCs = strictUCs.map(x => s"  def ${x.constrAttrName}: ${x.classRefName}")
            val inheritedUCs = ucs.filter(uc => !strictUCs.exists(_.selfRefByName === uc.selfRefByName)).map { x =>
                s"  override def ${x.constrAttrName}: ${x.classRefName}"
            }
            attrUCs ++ inheritedUCs
        }
        else {
            val attrUCs = strictUCs.map(x => s"  val ${x.constrAttrName}: ${x.classRefName} = ${x.classRefName}(${x.attrNames.map(x => s"${x}").mkString(", ")})")
            val inheritedUCs = ucs.filter(uc => !strictUCs.exists(_.selfRefByName === uc.selfRefByName)).map { x =>
                s"  override val ${x.constrAttrName}: ${x.classRefName} = ${x.classRefName}(${x.attrNames.map(x => s"${x}").mkString(", ")})"
            }
            attrUCs ++ inheritedUCs
        }
    }

    override def genFKRefAttrs(implicit resolver: SchemaDef): Seq[String] = {
        if (isAbstract) {
            val fkRefAttrs = strictFKs.withFilter(x => !resolver.enumClasses.exists(_.selfRef === x.referencedClassRef)).map { x =>
                val refUCCons = x.resolveUCConstraint
                val classNameUC = refUCCons.classRefNameRelative(group)
                s"  def ${x.constrAttrName}: ${if (x.isMandatory) classNameUC else s"Option[${classNameUC}]"}"
            }
            val inheritedRefFKAttrs = fks.filter(fk => !strictFKs.exists(_.selfRefByName === fk.selfRefByName) && !resolver.enumClasses.exists(_.selfRef === fk.referencedClassRef)).map { x =>
                val refUCCons = x.resolveUCConstraint
                val classNameUC = refUCCons.classRefNameRelative(group)
                s"  override def ${x.constrAttrName}: ${if (x.isMandatory) classNameUC else s"Option[${classNameUC}]"}"
            }
            fkRefAttrs ++ inheritedRefFKAttrs
        }
        else {
            val fkRefAttrs = strictFKs.withFilter(x => !resolver.enumClasses.exists(_.selfRef === x.referencedClassRef)).map(fkHelper(_, false))
            val inheritedRefFKAttrs = fks.filter(fk => !strictFKs.exists(_.selfRefByName === fk.selfRefByName) && !resolver.enumClasses.exists(_.selfRef === fk.referencedClassRef)).map(fkHelper(_, true))
            fkRefAttrs ++ inheritedRefFKAttrs
        }
    }

    override def genClassDefs(implicit resolver: SchemaDef): String = {
        val out = new StringBuilder()
        genClassDefsWithOutLob(out)
        //genClassDefs(false, out)
        out.toString().chmp
    }

    override def genClassDefsWithOutLob(out: StringBuilder)(implicit resolver: SchemaDef): Unit = {
        val attrs = (strictAttrs ++ (strictFKs.flatMap(_.attrNames).map(attr(_)))).filter(attr => attr.attrType != DomainClob && attr.attrType != DomainBlob)

        val calculatedAttrDefs = attrs.withFilter(_.isCalculated).map(x => s"  def ${x.name}: ${x.attrType.scalaTypeAsString(group, resolver)} = ${x.formula.get}")

        val allAttrs = attrNames(resolver).map(attr(_))

        val discrNames = discriminatorVals(resolver).map(_.toParentClass.discriminatorAttrName)


        val discrAttr = parentClassLink.toParentClass.discriminatorAttr(resolver)
        val discrDefString = s"  def ${discrAttr.name}: ${discrAttr.scalaTypeAsString(group, resolver)} = ${discrAttr.stringToSourceValue(parentClassLink.discriminatorValue)(group, resolver)}"

        val classDef: String =
            if (!isAbstract) {
                val attrDefs = allAttrs.withFilter(x => (!x.isCalculated) && (!discrNames.exists(_ === x.name))).map(x => s"  val ${x.name}: ${x.attrType.scalaTypeAsStringConditional(x.isMandatory)(group, resolver)}")

                val attr4ObjDefs = allAttrs.withFilter(x => (!x.isCalculated) && (!discrNames.exists(_ === x.name))).map(x => s"${x.name}: ${x.attrType.scalaTypeAsStringConditional(x.isMandatory)(group, resolver)}")
                val param4ObjDefs = allAttrs.withFilter(x => (!x.isCalculated) && (!discrNames.exists(_ === x.name))).map(x => s"${x.name} = ${x.name}")

                val (paramList, paramImpl) = genParamListAndImplWithOutLob


                val sortedAttrs = allAttrs.withFilter(x => (!x.isCalculated)).map(_.name).sortBy(s => s).zipWithIndex
                val productCanEquals = s"  override def canEqual(that: Any): Boolean = that.isInstanceOf[${className}]"
                val productArity = s"  override def productArity: Int = ${sortedAttrs.size}"
                val productElementCases = sortedAttrs.map { case (attrNm, pos) => s"    case ${pos} => ${attrNm}" }
                val productElement =
                    s"""|  override def productElement(n: Int): Any = n match {
                        |${productElementCases.mkString(newLine)}
                        |  }""".stripMargin

                s"""class ${className} (
                    |${attrDefs.mkString(",".newLine)}) extends ${parentClassLink.link.toString(selfRef)} {
                    |${discrDefString}
                    |${calculatedAttrDefs.mkString(newLine)}
                    |${genUCAttrs.mkString(newLine)}
                    |${genFKRefAttrs.mkString(newLine)}
                    |
                |${productCanEquals}
                    |${productArity}
                    |${productElement}
                    |}""".stripMargin.newLine.newLine +
                  s"""object ${className} {
                      |  def apply(${attr4ObjDefs.mkString(", ")}) = new ${className}(${param4ObjDefs.mkString(", ")})
                      |  def apply(${paramList}) = new ${className}(${paramImpl})
                      |}""".stripMargin
            }
            else {

                val attrDefs = attrs.withFilter(!_.isCalculated).map(x => s"  def ${x.name}: ${x.attrType.scalaTypeAsStringConditional(x.isMandatory)(group, resolver)}")

                s"""|trait ${className}  extends ${parentClassLink.link.toString(selfRef)} {
                    |${attrDefs.mkString(newLine)}
                    |${discrDefString}
                    |${calculatedAttrDefs.mkString(newLine)}
                    |${genUCAttrs.mkString(newLine)}
                    |${genFKRefAttrs.mkString(newLine)}
                    |}
                    |""".stripMargin

            }

        val ucRefs = genUCRefs

        out append fill("from ChildHierarchyClassDefMetaGen").newLine

        out append ucRefs.mkString(newLine).newLine
        out append classDef
        out append newLine
        out append fill("end from ChildHierarchyClassDefMetaGen").newLine
    }

    //Пример реализации в модуле AbstractClassDefMetaGen
    override def genClassDefsWithLob(out: StringBuilder)(implicit resolver: SchemaDef): Unit = ???

}
