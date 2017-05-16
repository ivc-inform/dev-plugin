package ru.simplesys.plugins
package sourcegen
package meta

//import ru.simplesys.meta.types.StringAddons
import com.simplesys.common.Strings._
import com.simplesys.common.equality.SimpleEquality._

/*trait HierarchyClassDefMetaGen {
  self: HierarchyClassDef =>

}*/


trait HierarchyClassDefWSettingMetaGen extends AbstractClassDefMetaGen {
  self: HierarchyClassDef with HierarchyLevelSetting with AbstractClassDefMetaGen =>

  override def genUCAttrs(implicit resolver: SchemaDef): Seq[String] = {
    val attrUCs = strictUCs.map(x => s"  def ${x.constrAttrName}: ${x.classRefName}")
    attrUCs
  }


  override def genFKRefAttrs(implicit resolver: SchemaDef): Seq[String] = {
    val fkRefAttrs = strictFKs.withFilter(x => !resolver.enumClasses.exists(_.selfRef === x.referencedClassRef)).map {x =>
      val refUCCons = x.resolveUCConstraint
      val classNameUC = refUCCons.classRefNameRelative(group)
      s"  def ${x.constrAttrName}: ${if (x.isMandatory) classNameUC else s"Option[${classNameUC}]"}"
    }
    fkRefAttrs
  }

  override def genClassDefs(implicit resolver: SchemaDef): String = {
    val out = new StringBuilder()

    //out append genCompanionObject(resolve)
    val classDef =
      if (!isAbstract) super.genClassDefs(resolver)
      else {
        val attrs = strictAttrs ++ (strictFKs.flatMap(_.attrNames).map(attr(_)))

        val calculatedAttrDefs = attrs.withFilter(_.isCalculated).map(x => s"  def ${x.name}: ${x.attrType.scalaTypeAsString(group, resolver)} = ${x.formula.get}")

        val attrDefs = attrs.withFilter(!_.isCalculated).map(x => s"  def ${x.name}: ${x.attrType.scalaTypeAsStringConditional(x.isMandatory)(group, resolver)}")

        val ucRefs = genUCRefs

        out append ucRefs.mkString(newLine).newLine

        val ucAttrs = genUCAttrs

        s"""|trait ${className} extends Product {
            |${attrDefs.mkString(newLine)}
            |${calculatedAttrDefs.mkString(newLine)}
            |${ucAttrs.mkString(newLine)}
            |${genFKRefAttrs.mkString(newLine)}
            |}
            |""".stripMargin

      }

    out append classDef
    out.toString().chmp
  }


}
