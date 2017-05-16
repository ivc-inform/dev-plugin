package ru.simplesys.plugins
package sourcegen
package balsamiq

import com.simplesys.common.Strings._
import scalaz.syntax.std.option._

//--Callout aka Callout or (possibly) indirect DataSourceRef---------------------------------------------------------
trait CalloutControl extends MockupControl with NonUILikeControlImpl {
  self: MockupControlData =>
}

trait MockupDataSourceRefControl extends MockupControl with NonUILikeControlImpl {
  self: MockupControlData =>
  val dataSourceRef: String
  def resolveDataSourceRef(viaMockup: Mockup): DataSourceLink = {
    val stringLink = dataSourceRef match {
      case rx"(\d.*)${reference}" => viaMockup.dataSourceMap(reference)
      case inplaceName => inplaceName
    }
    DataSourceLink(stringLink)
  }
}
//--Callout aka Callout or (possibly) indirect DataSourceRef---------------------------------------------------------


//--StickyNote aka StickyNote or (possibly) data source map and other metadata storage-------------------------------
trait StickyNoteControl extends MockupControl with NonUILikeControlImpl {
  self: MockupControlData =>
}

trait MockupDataSourceMapControl extends MockupControl with NonUILikeControlImpl {
  self: MockupControlData =>
  val dataSourceMappingList: Seq[String]

//TODO
  val dataSourceMap: Map[String, String] = dataSourceMappingList.map(x => {val p = x.split(":"); (p.head.trim, p.last.trim)})(collection.breakOut)
}

//--StickyNote aka StickyNote or (possibly) data source map and other metadata storage-------------------------------


//--TabBar-----------------------------------------------------------------------------------------------------------
trait TabBarControl extends MockupControl with CanvasLikeControlImpl {
  self: MockupControlData =>
  // we take the last URL from string like <name>&bm;<viewURL>&bm;<loadURL>&bm;<editURL>
  val contentLinks: IndexedSeq[Option[String]] = properties.flatMap(_.hrefs.map {str => str.split(',').map(_.split("""&bm;""").last).map(Option(_).filter(_.trim.nonEmpty)).toIndexedSeq}) getOrElse IndexedSeq()
  val selectedIndex: Option[Int] = properties.flatMap(_.selectedIndex)
  val captions: IndexedSeq[Option[String]] = {
    val regexSplit = """(?<!\\),""".r
    text.map(regexSplit.split(_).map(x => Option(x.trim).filter(_.nonEmpty))) | Array()
  }

  override def toUIControl(uiProps: CanvasProps)(implicit currentMockup: Mockup, resolver: UIGenerator) = {
    val tabs: Array[Option[IUIControl]] = contentLinks.map(_.flatMap(x => {
      val currMockup = resolver.resolveMockup(x)
      currMockup.toUIControl(CanvasProps(widthPCT = 100.some, heightPCT = 100.some))(resolver)
    })).toArray

//a little bit of pretty old imperative style. If tab not linked, it's filled with inline controls
    val currentTab = super[CanvasLikeControlImpl].toUIControl(CanvasProps(widthPCT = 100.some, heightPCT = 100.some))
    selectedIndex.foreach(i => if (tabs(i).isEmpty) tabs(i) = currentTab)

    val result: Seq[Tab] = (tabs zip captions) map {case (t, c) => new Tab(None, c | "", t)}

    Some(new TabSet(dsLink, result, selectedIndex)(uiProps))
  }

}
//--TabBar-----------------------------------------------------------------------------------------------------------

//--DynamicForm------------------------------------------------------------------------------------------------------
trait DynamicFormControl extends MockupControl with DynamicFormLikeControlImpl {
  self: MockupControlData =>
}
//--DynamicForm------------------------------------------------------------------------------------------------------


//--FormItem---------------------------------------------------------------------------------------------------------
trait FormItemControl extends MockupControl {
  self: MockupControlData =>
  override def isValidDynamicFormControl: Boolean = true
  def bindedAttr: Option[String] = text.filter(_.startsWith("attr:")).map {x =>
    x.substring("attr:".length).trim
  }
  def isBindedToAttr: Boolean = bindedAttr.isDefined

  override def toUIControl(uiProps: CanvasProps)(implicit currentMockup: Mockup, resolver: UIGenerator) = None
}
//--FormItem---------------------------------------------------------------------------------------------------------

//--Splitters--------------------------------------------------------------------------------------------------------
trait HSplitterControl extends MockupControl with NonUILikeControlImpl {
  self: MockupControlData =>
  override def canBeResizedInXRight: Boolean = false
}

trait VSplitterControl extends MockupControl with NonUILikeControlImpl {
  self: MockupControlData =>
  override def canBeResizedInYBottom: Boolean = false
}
//--Splitters--------------------------------------------------------------------------------------------------------

//--DataGrid---------------------------------------------------------------------------------------------------------
trait DataGridControl extends MockupControl {
  self: MockupControlData =>
  override def toUIControl(uiProps: CanvasProps)(implicit currentMockup: Mockup, resolver: UIGenerator) = new ListGrid(dsLink, None)(uiProps).some
}
//--DataGrid---------------------------------------------------------------------------------------------------------