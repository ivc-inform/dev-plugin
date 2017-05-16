package ru.simplesys.plugins
package sourcegen
package meta

case class AttrToColumnMapping(attrLink: LinkRefAttr,
                               columnLink: LinkRefCol) {
  def attr(implicit resolver: SchemaDef): AttrDef[_] = attrLink.toAttr
  def column(implicit resolver: SchemaDef): ColumnDef[_] = columnLink.toCol
}

/*
trait ConstraintMapping {
  def classConstraintRef: LinkRefToClassConstraint
  def implConstraint: LinkRefToConstraintImpl
  def implDetails: ConstraintImplShortDetails
}
*/

case class ConstraintMapping(classConstraintRef: LinkRefToClassConstraint, implConstraint: LinkRefToConstraintImpl, implDetails: ConstraintImplShortDetails)

/*читаем хардкоденные маппинги
читаем хардкоденные таблицы (2 типа FK - в классы и в таблицы + функция получения всех FK в таблицы)
генерируем маппинги для таблиц с автосозданием
генерируем таблицы
*/
