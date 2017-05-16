package ru.simplesys.plugins
package sourcegen
package balsamiq

import scala.xml.{NodeSeq, Null, Elem}
import ru.simplesys.plugins.sourcegen.meta.SealedEnum
import ru.simplesys.meta.types.Locator
import scalaz.syntax.std.option._

//--Alignments--------------------------------------------------------------------------
sealed trait AbsAlignmentType extends SealedEnum

trait VerticalAlignmentType extends AbsAlignmentType {
  self: AbsAlignmentType =>
}

trait HorizontalAlignmentType extends AbsAlignmentType {
  self: AbsAlignmentType =>
}

case object AlignmentTop extends AbsAlignmentType with VerticalAlignmentType {
  val key = "top"
}

case object AlignmentLeft extends AbsAlignmentType with HorizontalAlignmentType {
  val key = "left"
}

case object AlignmentRight extends AbsAlignmentType with HorizontalAlignmentType {
  val key = "right"
}

case object AlignmentBottom extends AbsAlignmentType with VerticalAlignmentType {
  val key = "bottom"
}

case object AlignmentCenter extends AbsAlignmentType with VerticalAlignmentType with HorizontalAlignmentType {
  val key = "center"
}

//--Alignments--------------------------------------------------------------------------

//--SelectionStyle--------------------------------------------------------------------------
sealed trait SelectionStyle extends SealedEnum

case object SelectionStyleSimple extends SelectionStyle {
  val key = "Simple"
}

case object SelectionStyleMultiple extends SelectionStyle {
  val key = "Multiple"
}

case object SelectionStyleSingle extends SelectionStyle {
  val key = "Single"
}

//--SelectionStyle--------------------------------------------------------------------------

//--TextMatchStyleEnum--------------------------------------------------------------------------
sealed trait TextMatchStyleEnum extends SealedEnum

case object TextMatchStyleExact extends TextMatchStyleEnum {
  val key = "txtMchStyleExact"
}

case object TextMatchStyleSubstring extends TextMatchStyleEnum {
  val key = "txtMchStyleSubstring"
}

case object TextMatchStyleStartWith extends TextMatchStyleEnum {
  val key = "txtMchStyleStartWith"
}

//--TextMatchStyleEnum--------------------------------------------------------------------------

//--SortDirectionEnum--------------------------------------------------------------------------
sealed trait SortDirectionEnum extends SealedEnum

case object SortDirectionAsc extends SortDirectionEnum {
  val key = "srtDirAscending"
}

case object SortDirectionDesc extends SortDirectionEnum {
  val key = "srtDirDescending"
}

//--SortDirectionEnum--------------------------------------------------------------------------

//--SortSettings--------------------------------------------------------------------------
case class SortSpecifier(val property: String, val sortDirection: SortDirectionEnum) {
  def toXML = {
    <Property>{property}</Property>
    <SortDirection>{sortDirection.toString}</SortDirection>
  }
}

case class SortSpecifierArray(val sortSpecifier: Seq[SortSpecifier]) {
  def toXML = sortSpecifier.map(x =>
    <SortSpecifier>
      {x.toXML}
    </SortSpecifier>)
}

//--SortSettings--------------------------------------------------------------------------

trait CanvasPropsBase {
  def canvasProps: CanvasProps
  def width: Option[String] = canvasProps.plainWidth orElse canvasProps.widthPCT.map(_.toString + "%")
  def height: Option[String] = canvasProps.plainHeight orElse canvasProps.heightPCT.map(_.toString + "%")
  def showResizeBar: Option[Boolean] = canvasProps.showResizeBar
  def resizeBarTarget: Option[String] = canvasProps.resizeBarTarget
}

case class CanvasProps(showResizeBar: Option[Boolean] = None, resizeBarTarget: Option[String] = None, widthPCT: Option[Int] = None, heightPCT: Option[Int] = None, plainWidth: Option[String] = None, plainHeight: Option[String] = None, direction: Option[CurrentDirection] = None)

trait CanvasPropsNoCanvas {
  def width: Option[String] = None
  def height: Option[String] = None
  def showResizeBar: Option[Boolean] = None
  def resizeBarTarget: Option[String] = None
}


trait UIControl extends ControlTree {
  self: ControlTree =>
  def toXML: Elem

  def canvasPropsToXML: NodeSeq = {
    width.map(x => <Width>{x}</Width>).toSeq ++
    height.map(x => <Height>{x}</Height>).toSeq ++
    showResizeBar.map(x => <ShowResizeBar>{x}</ShowResizeBar>).toSeq ++
    resizeBarTarget.map(x => <ResizeBarTarget>{x}</ResizeBarTarget>).toSeq
  }

  def width: Option[String]
  def height: Option[String]
  def showResizeBar: Option[Boolean]
  def resizeBarTarget: Option[String]

}

trait LayoutBase extends UIControl {
  self: ControlTree =>
  def align: Option[AbsAlignmentType]
  def members: Option[Seq[UIControl]]
  def isVertical: Boolean
}

trait HLayoutBase extends LayoutBase {
  self: ControlTree =>
  override def align: Option[HorizontalAlignmentType]
  override def isVertical: Boolean = false
}

trait VLayoutBase extends LayoutBase {
  self: ControlTree =>
  override def align: Option[VerticalAlignmentType]
  override def isVertical: Boolean = true
}

trait LayoutAsContainer {
  self: LayoutBase =>
  def children: Seq[IUIControl] = members | Seq()
}

class Layout(val ds: Option[DataSourceLink], val align: Option[AbsAlignmentType], val members: Option[Seq[UIControl]], val isVertical: Boolean)(val canvasProps: CanvasProps) extends LayoutBase with ControlTreeContainer with LayoutAsContainer with CanvasPropsBase {
  override def toXML: Elem = {
    <Layout>
      {align.map(x => <Align>{x.toString}</Align>).orNull}
      {canvasPropsToXML}
      <Vertical>{isVertical.toString}</Vertical>
      {members.map(x => <Members>
        {x.map(_.toXML)}
      </Members>).orNull}
    </Layout>
  }
}

class HLayout(val ds: Option[DataSourceLink], val align: Option[HorizontalAlignmentType], val members: Option[Seq[UIControl]])(val canvasProps: CanvasProps) extends HLayoutBase with ControlTreeContainer with LayoutAsContainer with CanvasPropsBase {
  override def toXML: Elem = {
    <HLayout>
      {align.map(x => <Align>{x.toString}</Align>).orNull}
      {canvasPropsToXML}
      {members.map(x =>
      <Members>
        {x.map(_.toXML)}
      </Members>).orNull}
    </HLayout>
  }
}

class VLayout(val ds: Option[DataSourceLink], val align: Option[VerticalAlignmentType], val members: Option[Seq[UIControl]])(val canvasProps: CanvasProps) extends VLayoutBase with ControlTreeContainer with LayoutAsContainer with CanvasPropsBase {
  override def toXML: Elem = {
//    val canvasProps = canvasPropsToXML
    <VLayout>
      {align.map(x => <Align>{x.toString}</Align>).orNull}
      {canvasPropsToXML}
      {members.map(x =>
      <Members>
        {x.map(_.toXML)}
      </Members>).orNull}
    </VLayout>
  }
}

trait BaseGridBase extends VLayoutBase {
  self: ControlTreeSimple with DataBound =>
  def autoFetchData: Option[Boolean]
  def canAcceptDroppedRecords: Option[Boolean]
  def canEdit: Option[Boolean]
  def canReorderRecords: Option[Boolean]
  def canReparentNodes: Option[Boolean]
  def canSelectCells: Option[Boolean]
  def dataPageSize: Option[String]
  def drawAheadRatio: Option[String]
  def editByCell: Option[Boolean]
  def fetchDelay: Option[Int]
  def dataFetchMode: Option[String]
  def initialSort: Option[SortSpecifierArray]
  def filterOnKeypress: Option[Boolean]
  def selectionType: Option[SelectionStyle]
  def showFilterEditor: Option[Boolean]
  def textMatchStyle: Option[TextMatchStyleEnum]
  override def members: Option[Seq[UIControl]] = None
}

class ListGrid(val ds: Option[DataSourceLink],
               val align: Option[VerticalAlignmentType],
               val autoFetchData: Option[Boolean] = None,
               val canAcceptDroppedRecords: Option[Boolean] = None,
               val canEdit: Option[Boolean] = None,
               val canReorderRecords: Option[Boolean] = None,
               val canReparentNodes: Option[Boolean] = None,
               val canSelectCells: Option[Boolean] = None,
               val dataPageSize: Option[String] = None,
               val drawAheadRatio: Option[String] = None,
               val editByCell: Option[Boolean] = None,
               val fetchDelay: Option[Int] = None,
               val dataFetchMode: Option[String] = None,
               val initialSort: Option[SortSpecifierArray] = None,
               val filterOnKeypress: Option[Boolean] = None,
               val selectionType: Option[SelectionStyle] = None,
               val showFilterEditor: Option[Boolean] = None,
               val textMatchStyle: Option[TextMatchStyleEnum] = None
             )(val canvasProps: CanvasProps) extends BaseGridBase with ControlTreeSimple with DataBound with CanvasPropsBase {
  override def toXML: Elem = {
    <ListGrid>
      {super[DataBound].toXML}
      {align.map(x => <Align>{x.toString}</Align>).orNull}
      {canvasPropsToXML}
    </ListGrid>
  }
}

class Tab(val ds: Option[DataSourceLink], val title: String, val pane: Option[UIControl]) extends UIControl with ControlTreeContainer with CanvasPropsNoCanvas {
  def children: Seq[IUIControl] = pane.toSeq
  override def toXML: Elem = {
    <Tab>
      <Title>{title}</Title>
      {pane.map(x => <Pane>{x.toXML}</Pane>).orNull}
    </Tab>
  }
}

class TabSet(val ds: Option[DataSourceLink], val tabs: Seq[Tab], val selectedTab: Option[Int])(val canvasProps: CanvasProps) extends UIControl with ControlTreeContainer with CanvasPropsBase {
  def children: Seq[UIControl] = tabs
  override def toXML: Elem = {
    <TabSet>
      {if (!tabs.isEmpty)
        <Tabs>
          {tabs.map(_.toXML)}
        </Tabs>
      else Null}
      {selectedTab.map(x => <SelectedTab>{x.toString}</SelectedTab>).orNull}
      {canvasPropsToXML}
    </TabSet>
  }
}

case class DynamicFormField(name: String, rowSpan: Option[Int], colSpan: Option[Int], startRow: Option[Boolean], endRow: Option[Boolean], numStartRow: Int, numStartCol: Int) {
  def toXML: Elem = {
    <Field name={name} numStartRow={numStartRow.toString} numStartCol={numStartCol.toString} rowSpan={rowSpan.map(_.toString).orNull} colSpan={colSpan.map(_.toString).orNull} startRow={startRow.map(_.toString).orNull} endRow={endRow.map(_.toString).orNull}/>
  }
}

sealed trait TitleOrientation

case object TitleOrientationTop extends TitleOrientation {
  override def toString = "top"
}

case object TitleOrientationLeft extends TitleOrientation {
  override def toString = "left"
}

case object TitleOrientationRight extends TitleOrientation {
  override def toString = "right"
}

class DynamicForm(val ds: Option[DataSourceLink], val numCols: Integer, val numRows: Integer, val bindedFields: Seq[DynamicFormField])(val canvasProps: CanvasProps) extends UIControl with ControlTreeSimple with DataBound with CanvasPropsBase {
  def titleOrientation: TitleOrientation = TitleOrientationTop
  def padding: Option[Int] = 3.some

  override def width: Option[String] = canvasProps.direction match {
    case Some(TopToBottomDirection) => "*".some
    case Some(LeftToRightDirection) => None
    case None => None
  }

  override def height: Option[String] = canvasProps.direction match {
    case Some(TopToBottomDirection) => None
    case Some(LeftToRightDirection) => "*".some
    case None => None
  }

  override def resizeBarTarget: Option[String] = None

  override def toXML: Elem = {
    <DynamicForm>
      {super[DataBound].toXML}
      <NumCols>{numCols}</NumCols>
      <NumRows>{numRows}</NumRows>
      <TitleOrientation>{titleOrientation.toString}</TitleOrientation>
      {padding.map(x => <Padding>{x.toString}</Padding>).orNull}
      {canvasPropsToXML}
      {if (bindedFields.nonEmpty)
      <Fields>
        {bindedFields.map(_.toXML)}
      </Fields> else Null}
    </DynamicForm>
  }
}


trait DataBound {
  def ds: Option[DataSourceLink]
  def toXML: Elem = {
    ds.map(x => <DataSource>{x.toString}</DataSource>).orNull
  }
}


sealed trait DataSourceNamespace extends SealedEnum

case object DataSourceNamespaceBO extends DataSourceNamespace {
  val key = "BO"
}

case object DataSourceNamespaceDS extends DataSourceNamespace {
  val key = "DS"
}

case class DataSourceLink(val group: Locator, val namespace: DataSourceNamespace, val name: String) {
  override def toString: String = s"${group}.${name}"
}

object DataSourceLink {
  def apply(str: String): DataSourceLink = {
    //TODO!
    DataSourceLink("", DataSourceNamespaceBO, str)
  }
}

trait ControlTree {
  self: UIControl =>
  def ds: Option[DataSourceLink]
//  def effectiveDS(parentDSs: Seq[DataSourceLink]): Option[DataSourceLink] = (parentDSs ++ ds).lastOption
}

trait ControlTreeContainer extends ControlTree {
  self: UIControl =>
  def children: Seq[ControlTree]
}

trait ControlTreeSimple extends ControlTree {
  self: UIControl =>
//  def effectiveDS(parentDSs: Seq[DataSourceLink]): Option[DataSourceLink] = (parentDSs ++ ds).lastOption
}