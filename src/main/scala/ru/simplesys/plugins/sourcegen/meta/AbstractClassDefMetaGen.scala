package ru.simplesys.plugins
package sourcegen
package meta

//import ru.simplesys.meta.types.StringAddons
import com.simplesys.common.Strings._
import com.simplesys.scalaGen.{ScalaEndComment, ScalaComment}
import com.simplesys.common.equality.SimpleEquality._

trait AbstractClassDefMetaGen {
    self: AbstractClassDef =>

    def genObjectsForClass(implicit resolver: SchemaDef): String = {
        val out = new StringBuilder()

        val enumAttrs = strictAttrs.collect {
            case x: EnumAttrDef[_] => x
        }

        out append enumAttrs.map(_.genEnumObjectsForClass).mkString("\n\n")

        out append {
            this match {
                case x: EnumProviderMetaGen => x.genEnumObjectsForClass
                case _ => ""
            }
        }

        out.toString().chmp
    }

    def genUCRefs (implicit resolver: SchemaDef): Seq[String] = {
      val ucRef = strictUCs.map {x =>
        val ucAttrs = x.attrs.map {x =>
          s"""val ${x.name}: ${x.scalaTypeAsString(group, resolver)}"""
        }.mkString(", ")

        val ucAttrsParams = x.attrs.map {x =>
          s"""${x.name}: ${x.scalaTypeAsString(group, resolver)}"""
        }.mkString(", ")

        s"""|class ${x.classRefName}(${ucAttrs}) extends BOReference[${className.capitalize}]
            |
            |object ${x.classRefName} {
            |  def apply(${ucAttrsParams}): ${x.classRefName} = new ${x.classRefName}(${x.attrNames.mkString(", ")})
            |}
            |""".stripMargin
      }

      ucRef
    }


    def genUCAttrs(implicit resolver: SchemaDef): Seq[String] = {
      val attrUCs = strictUCs.map(x => s"  val ${x.constrAttrName}: ${x.classRefName} = ${x.classRefName}(${x.attrNames.map(x => s"${x}").mkString(", ")})")
      attrUCs
    }

    protected def fkHelper(x: ForeignKeyConstraintDef, isOverride: Boolean)(implicit resolver: SchemaDef): String = {
      val refUCCons = x.resolveUCConstraint
      val classNameUC = refUCCons.classRefNameRelative(group)
      val evalList = if (!x.isMandatory) {
        if (x.attrNames.size === 1)s"${x.attrNames.head}.map(${classNameUC}(_))"
        else {
          val forNames = x.attrNames.map(a => x.attrMapping(a)).filter {case (mapp, refAttr) => refAttr.isMandatory}.map{ case (mapp, refAttr) => mapp.localName}
          s"for (${forNames.map(a => s"for${a.capitalize} <- ${a}").mkString("; ")}) yield ${classNameUC}(${x.attrNames.map(a => if (forNames.exists(_ === a)) s"for${a.capitalize}" else a).mkString(", ")})"
        }
      }
      else s"${classNameUC}(${x.attrNames.map(x => s"${x}").mkString(", ")})"

      s"  ${if (isOverride) "override " else ""}val ${x.constrAttrName}: ${if (x.isMandatory) classNameUC else s"Option[${classNameUC}]"} = ${evalList}"
    }

    def genFKRefAttrs(implicit resolver: SchemaDef): Seq[String] = {
      val fkRefAttrs = strictFKs.withFilter(x => !resolver.enumClasses.exists(_.selfRef === x.referencedClassRef)).map(fkHelper(_, false))
      fkRefAttrs
    }


    protected def genParamListAndImpl(implicit resolver: SchemaDef): (String, String) = {
      val discrNames = discriminatorVals(resolver).map(_.toParentClass.discriminatorAttrName)
      val attrsNotCalculated = attrNames(resolver).map(attr(_)).filter(x => (!x.isCalculated) && (!discrNames.exists(_ === x.name)))//attrs.filter(!_.isCalculated)

      val fksWithoutEnums = fks.filter(x => !resolver.enumClasses.exists(_.selfRef === x.referencedClassRef))

      val attrNamesFromFKs: Map[String, ForeignKeyConstraintDef] = fksWithoutEnums.flatMap(x => x.attrNames.map(y => (y, x)))(collection.breakOut)
      val attrNamesFromPK = pk.attrNames.toSet
      val exclusionAttrs = attrNamesFromFKs.keySet union attrNamesFromPK

      val paramListDefsForObjDefs = {Seq(s"pk: ${pk.classRefName}") ++ attrsNotCalculated.filterNot(x => exclusionAttrs.contains(x.name)).map(x => s"${x.name}: ${x.scalaTypeAsString(group, resolver)}") ++ fksWithoutEnums.map(f => s"${f.constrAttrName}: ${f.resolveUCConstraint.classRefNameRelative(this.group)}")}.mkString(", ")
      val paramListImplsForObjDefs = attrsNotCalculated.map(a => {a.name match {
        case x if attrNamesFromPK.contains(x) => s"${x} = pk.${x}"
        case x if attrNamesFromFKs.contains(x) => s"${x} = ${val currFK = attrNamesFromFKs(x)
          val temp = currFK.attrMapping(x)
          val mapp = temp._1
          val refAttr = temp._2
          if (currFK.isMandatory) s"${currFK.constrAttrName}.${refAttr.name}"
          else {if (refAttr.isMandatory) s"Some(${currFK.constrAttrName}).map(_.${refAttr.name})" else s"${currFK.constrAttrName}.flatMap(_.${refAttr.name})"}}"
        case x => s"${x} = ${x}"
      }
      }).mkString(", ")
      (paramListDefsForObjDefs, paramListImplsForObjDefs)
    }

    def genClassDefs(implicit resolver: SchemaDef): String = {
      val out = new StringBuilder()

      //out append genCompanionObject(resolve)

      val attrs = strictAttrs ++ (strictFKs.flatMap(_.attrNames).map(attr(_)))

      val attrDefs = attrs.withFilter(!_.isCalculated).map(x => s"  val ${x.name}: ${x.scalaTypeAsString(group, resolver)}")

      val calculatedAttrDefs = attrs.withFilter(_.isCalculated).map(x => s"  def ${x.name}: ${x.scalaTypeAsString(group, resolver)} = ${x.formula.get}")

      out append fill("from AbstractClassDefMetaGen").newLine

      val ucRefs = genUCRefs.mkString(newLine).newLine
      val ucAttrs = genUCAttrs

      out append ucRefs

      val sortedAttrs = attrs.withFilter(!_.isCalculated).map(_.name).sortBy(s => s).zipWithIndex
      val productCanEquals = s"  override def canEqual(that: Any): Boolean = that.isInstanceOf[${className.capitalize}]"
      val productArity = s"  override def productArity: Int = ${sortedAttrs.size}"
      val productElementCases = sortedAttrs.map { case (attrNm, pos) => s"    case ${pos} => ${attrNm}"}
      val productElement = s"""|  override def productElement(n: Int): Any = n match {
                               |${productElementCases.mkString(newLine)}
                               |  }""".stripMargin


        val classDef = s"""|class ${className.capitalize} (
                           |${attrDefs.mkString(",".newLine)}) extends Product {
                           |${calculatedAttrDefs.mkString(newLine)}
                           |${ucAttrs.mkString(newLine)}
                           |${genFKRefAttrs.mkString(newLine)}
                           |
                           |${productCanEquals}
                           |${productArity}
                           |${productElement}
                           |}
                           |""".stripMargin

        out append classDef

        val attrsNotCalculated = attrs.filter(!_.isCalculated)

        val attr4ObjDefs = attrsNotCalculated.map(x => s"${x.name}: ${x.scalaTypeAsString(group, resolver)}")
        val param4ObjDefs = attrsNotCalculated.map(x => s"${x.name} = ${x.name}")

        val (paramList, paramImpl) = genParamListAndImpl


        val objDef = s"""|object ${className.capitalize} {
                         |   def apply(${attr4ObjDefs.mkString(",".space)}) = new ${className.capitalize}(${param4ObjDefs.mkString(",".space)})
                         |   def apply(${paramList}) = new ${className.capitalize}(${paramImpl})
                         |}
                         |""".stripMargin
        out append newLine
        out append objDef
        out append fill("end from AbstractClassDefMetaGen").newLine

        out.toString().chmp
    }


    def genClassMeta(implicit resolver: SchemaDef): String = {
        val out = new StringBuilder()

        val metaDef = s"""|object ${classMetaName} {
                      |
                      |  def insert() = {}
                      |  def insertAll() = {}
                      |  def delete() = {}
                      |  def deleteAll() = {}
                      |  def selectByID() = {}
                      |
                      |}""".stripMargin

        out append metaDef

        out.toString().chmp
    }
}
