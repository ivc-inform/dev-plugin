package ru.simplesys.plugins
package sourcegen
package meta

import scala.xml._
import ru.simplesys.meta.types.Locator
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.common.XMLs._
import ru.simplesys.coreutil.SealedEnumRuntime

sealed trait FetchSizeSetting {
  def toXml: Elem
}
case class FetchAll(val all: Boolean) extends FetchSizeSetting {
  def toXml: Elem = {
    <fetch>
      <fetchAll>{all}</fetchAll>
    </fetch>
  }
}
case class FetchSize(val size: Int) extends FetchSizeSetting {
  def toXml: Elem = {
    <fetch>
      <size>{size}</size>
    </fetch>
  }
}

object FetchSizeSetting {
  def apply(x: Node): FetchSizeSetting = {
    val size = (x \ "size").map(_.text.toInt).headOption.map(FetchSize(_))
    val all = (x \ "fetchAll").map(_.text.toBoolean).headOption.map(FetchAll(_))
    (size orElse all).get
  }
}


sealed trait OrderByModifier extends SealedEnum

object OrderByModifier {
  val modifiers: Map[String, OrderByModifier] = SealedEnumRuntime.values[OrderByModifier].map(x => (x.toString, x))(collection.breakOut)

  def apply(x: String): OrderByModifier = modifiers(x)
}

case object OrderByModifierASC extends OrderByModifier {
  override val key = "asc"
}

case object OrderByModifierDESC extends OrderByModifier {
  override val key = "desc"
}


sealed trait OrderByNullsFirstLast extends SealedEnum

object OrderByNullsFirstLast {
  val nullFirstLast: Map[String, OrderByNullsFirstLast] = SealedEnumRuntime.values[OrderByNullsFirstLast].map(x => (x.toString, x))(collection.breakOut)

  def apply(x: String): OrderByNullsFirstLast = nullFirstLast(x)
}

case object OrderByNullsLast extends OrderByNullsFirstLast {
  override val key = "nullsLast"
}

case object OrderByNullsFirst extends OrderByNullsFirstLast {
  override val key = "nullsFirst"
}

case object OrderByNullsDefault extends OrderByNullsFirstLast {
  override val key = "default"
}


case class OrderBySetting(val attrName: String, val modifier: OrderByModifier, val nullsFirstLast: OrderByNullsFirstLast) {
  def toXml: Elem = {
      <field attrName={attrName} by={Some(modifier).filter(_ === OrderByModifierDESC).map(_.toString).orNull} nulls={Some(nullsFirstLast).filter(_ !== OrderByNullsDefault).map(_.toString).orNull}/>
  }
}

object OrderBySetting {
  def apply(x: Node): OrderBySetting = {
    val attrName = (x \ "@attrName").text
    val modifier = (x \ "@by").textOption.map(OrderByModifier(_)).getOrElse(OrderByModifierASC)
    val nullsFirstLast = (x \ "@nulls").textOption.map(OrderByNullsFirstLast(_)).getOrElse(OrderByNullsDefault)
    OrderBySetting(attrName, modifier, nullsFirstLast)
  }
}

case class OrderByList(val orderList: Seq[OrderBySetting]) {
  def toXml: Elem = {
    <orderBy>
      {orderList.map(_.toXml)}
    </orderBy>
  }
}

object OrderByList {
  def apply(x: Node): OrderByList = {
    val list = (x \ "field").map(OrderBySetting(_))
    OrderByList(list)
  }
}

//---------------------------------------------------------------------------------

case class UISettings(val classRef: LinkRefToAbstractClass,
                      val uiFormType: UIFormType,
                      val fetchSize: Option[FetchSizeSetting],
                      val orderBy: Option[OrderByList]) {
  def orderByResult(implicit resolver: SchemaDef): Option[OrderByList] = orderBy orElse Some(OrderByList(classRef.toClass.pk.attrNames.map(x => OrderBySetting(x, OrderByModifierASC, OrderByNullsDefault)).toSeq))

  def toXml(implicit resolver: SchemaDef): Elem = {
    <uiSettings uiFormType={uiFormType.key}>
      {fetchSize.map(_.toXml).orNull}
      {orderByResult.map(_.toXml).orNull}
    </uiSettings>
  }
}

object UISettings {
  def apply(classRef: LinkRefToAbstractClass, x: Node): UISettings = {
    val uiFormType = UIFormType((x \ "@uiFormType").text)
    val orderBy = (x \ "orderBy").map(OrderByList(_)).headOption
    val fetchSize = (x \ "fetch").map(FetchSizeSetting(_)).headOption
    UISettings(classRef, uiFormType, fetchSize, orderBy)
  }

  def apply(parent: UISettings, child: UISettings): UISettings = UISettings(child.classRef, child.uiFormType, child.fetchSize, child.orderBy orElse parent.orderBy)
}

case class DefaultSettingsData(val classRef: LinkRefToAbstractClass, val showAttrsNames: Option[Seq[String]], val classifierAttrNames: Option[Seq[String]], val uiSettings: UISettings)

trait DefaultSettingsDef {
  self: DefaultSettingsData =>
  def classRef: LinkRefToAbstractClass

  def classifierAttrNames: Option[Seq[String]]

  def showAttrsNames: Option[Seq[String]]
  //def showAttrs(implicit resolver: SchemaDef): Option[Seq[AttrDef[_]]] = showAttrsNames.map(_.map(x => classRef.toClass.attr(x)))
  def showAttrsNamesResult(implicit resolver: SchemaDef): Seq[String] = showAttrsNames getOrElse classRef.toClass.attrNames
  def showAttrsResult(implicit resolver: SchemaDef): Seq[AttrDef[_]] = showAttrsNamesResult.map{x =>
    classRef.toClass.attr(x)
  }


  def classifierFK(implicit resolver: SchemaDef): Option[ForeignKeyConstraintDef] = classifierAttrNames.map {x =>
    LinkRefToClassForeignConstraintByAttrs(classRef, x.toSet).constraint
  }

  def uiSettings: UISettings

  def toXml(implicit resolver: SchemaDef): Elem = {
    //val attrsAll = showAttrsNamesResult
    <defaults>
      <showAttrs>
        {showAttrsNamesResult.map(y => <attrName>{y}</attrName>)}
      </showAttrs>
      {classifierAttrNames.map(x => <classifier>
      {x.map(y => <attrName>{y}</attrName>)}
    </classifier>).getOrElse(NodeSeq.Empty)}
      {uiSettings.toXml}
    </defaults>
  }
}

object DefaultSettings {
  def apply(classRef: LinkRefToAbstractClass, x: Node): IDefaultSettings = {
    val showAttrs = (x \ "showAttrs").map {x =>
      (x \ "attrName").map(_.text)
    }.headOption

    val classifier = (x \ "classifier").map {x =>
      (x \ "attrName").map(_.text)
    }.headOption

    val uiSettings = (x \ "uiSettings").map(UISettings(classRef, _)).head
    new DefaultSettingsData(classRef, showAttrs, classifier, uiSettings) with DefaultSettingsDef
  }

  def apply(parent: IDefaultSettings, child: IDefaultSettings): IDefaultSettings = new DefaultSettingsData(child.classRef, child.showAttrsNames orElse parent.showAttrsNames, child.classifierAttrNames orElse parent.classifierAttrNames, UISettings(parent.uiSettings, child.uiSettings)) with DefaultSettingsDef
}
