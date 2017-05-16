package ru.simplesys.plugins
package sourcegen
package balsamiq

import scala.xml.{Elem, XML, Node}
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.common.XMLs._
import com.simplesys.common.Strings._
import scalaz.syntax.std.option._

sealed abstract class CurrentDirection
case object LeftToRightDirection extends CurrentDirection
case object TopToBottomDirection extends CurrentDirection

trait Rectangle {
  self: RectangleBase =>
  def start(implicit direction: CurrentDirection): Int = direction match {
    case LeftToRightDirection => x
    case TopToBottomDirection => y
  }

  def end(implicit direction: CurrentDirection): Int = direction match {
    case LeftToRightDirection => right
    case TopToBottomDirection => bottom
  }

  def length(implicit direction: CurrentDirection): Int = direction match {
    case LeftToRightDirection => width
    case TopToBottomDirection => height
  }


  def x: Int
  def y: Int
  def width: Int
  def height: Int

  def right: Int
  def bottom: Int
  def square: Int = width * height

  def canBeResizedInXRight: Boolean = true
  def canBeResizedInYBottom: Boolean = true

  def isInside(container: Rectangle): Boolean = {
    if (container.x <= this.x && container.y <= this.y && container.bottom >= this.bottom && container.right >= this.right && container.square > this.square) true
    else false
  }

  protected def isSimpleOverlaps(withRect: Rectangle): Boolean = {
    val lt = if (withRect.y < bottom && withRect.y > y && withRect.x < x && withRect.right > x) true else false
    val lb = if (withRect.bottom < bottom && withRect.bottom > y && withRect.x < x && withRect.right > x) true else false
    val rt = if (withRect.y < bottom && withRect.y > y && withRect.x < right && withRect.right > right) true else false
    val rb = if (withRect.bottom < bottom && withRect.bottom > y && withRect.x < right && withRect.right > right) true else false
    lt || lb || rt || rb
  }

  def isOverlaps(withRect: Rectangle): Boolean = {
    val overl = isSimpleOverlaps(withRect) || withRect.isSimpleOverlaps(this)
    val ins = isInside(withRect) || withRect.isInside(this)
    overl || ins
  }

  def copy(x: Int = this.x, y: Int = this.y, right: Int = this.right, bottom: Int = this.bottom): Rectangle = RectangleByPoints(x, y, right, bottom)

  // maybe we need to respect canBeResizedInXRight and canBeResizedInYBottom
  def resizeChildToPossibleBorders(what: Rectangle, otherShapes: Seq[Rectangle]): Rectangle = {
    val toX = what.copy(x = this.x)
    val newX = if (otherShapes.forall(!_.isOverlaps(toX))) Some(this.x) else None
    val toY = what.copy(y = this.y)
    val newY = if (otherShapes.forall(!_.isOverlaps(toY))) Some(this.y) else None
    val toRight = what.copy(right = this.right)
    val newRight = if (otherShapes.forall(!_.isOverlaps(toRight))) Some(this.right) else None
    val toBottom = what.copy(bottom = this.bottom)
    val newBottom = if (otherShapes.forall(!_.isOverlaps(toBottom))) Some(this.bottom) else None

    // check func
    val check = {x: Rectangle => otherShapes.forall(!_.isOverlaps(x))}

    val newRect = (newX, newY, newRight,newBottom) match {
      // not really useful
      case (Some(xX), Some(yY), Some(rRight), Some(bBottom)) if check(what.copy(x = xX, y = yY, right = rRight, bottom = bBottom)) => what.copy(x = xX, y = yY, right = rRight, bottom = bBottom)

      // if we can resize in three ways
      case (Some(xX), Some(yY), Some(rRight), None) if check(what.copy(x = xX, y = yY, right = rRight)) => what.copy(x = xX, y = yY, right = rRight)
      case (None, Some(yY), Some(rRight), Some(bBottom)) if check(what.copy(y = yY, right = rRight, bottom = bBottom)) => what.copy(y = yY, right = rRight, bottom = bBottom)
      case (Some(xX), None, Some(rRight), Some(bBottom)) if check(what.copy(x = xX, right = rRight, bottom = bBottom)) => what.copy(x = xX, right = rRight, bottom = bBottom)
      case (Some(xX), Some(yY), None, Some(bBottom)) if check(what.copy(x = xX, y = yY, bottom = bBottom)) => what.copy(x = xX, y = yY, bottom = bBottom)

      // if we can resize in two orthogonal ways
      case (Some(xX), Some(yY), None, None) if check(what.copy(x = xX, y = yY)) => what.copy(x = xX, y = yY)
      case (None, Some(yY), Some(rRight), None) if check(what.copy(y = yY, right = rRight)) => what.copy(y = yY, right = rRight)
      case (None, None, Some(rRight), Some(bBottom)) if check(what.copy(right = rRight, bottom = bBottom)) => what.copy(right = rRight, bottom = bBottom)
      case (Some(xX), None, None, Some(bBottom)) if check(what.copy(x = xX, bottom = bBottom)) => what.copy(x = xX, bottom = bBottom)

      // can be moved on x - right
      case (Some(xX), None, xoRight, None) => what.copy(x = xX, right = xoRight | what.right)
      case (xoX, None, Some(xRight), None) => what.copy(x = xoX | what.x, right = xRight)
      // can be moved on y - bottom
      case (None, Some(yY), None, yoBottom) => what.copy(y = yY, bottom = yoBottom | what.bottom)
      case (None, yoY, None, Some(yBottom)) => what.copy(y = yoY| what.y, bottom = yBottom)

      // the same
      case (None, None, None, None) => what
    }
    newRect
  }
  
  def canChildResizeUpToXRight(who: Rectangle, others: Seq[Rectangle]): Boolean = {
    if (who.canBeResizedInXRight) {
      val newRect = who.copy(x = this.x, right = this.right)
      others.forall(!_.isOverlaps(newRect))
    } else false
  }

  def canChildResizeUpToYBottom(who: Rectangle, others: Seq[Rectangle]): Boolean = {
    if (who.canBeResizedInYBottom) {
      val newRect = who.copy(y = this.y, bottom = this.bottom)
      others.forall(!_.isOverlaps(newRect))
    } else false
  }

}

trait RectangleBase {
  self: Rectangle =>
}

trait RectangleByPointsBase extends RectangleBase {
  self: Rectangle =>
  override def width = right - x
  override def height = bottom - y
}

trait RectangleBySizeBase extends RectangleBase {
  self: Rectangle =>
  override def right = x + width
  override def bottom = y + height
}

case class RectangleByPoints(val x: Int, val y: Int, val right: Int, val bottom: Int) extends Rectangle with RectangleByPointsBase

trait RectangleShape extends Rectangle {
  self: RectangleBase =>

  def childrenInclMarkup(implicit currentMockup: Mockup): Seq[RectangleShape] = currentMockup.shapesMap(this)
  def children(implicit currentMockup: Mockup): Seq[RectangleShape] = currentMockup.shapesMapFiltered(this)

  def dsLink(implicit currentMockup: Mockup): Option[DataSourceLink] = currentMockup.shapesMap(this).collect{case x: MockupDataSourceRefControl => x}.headOption.map(_.resolveDataSourceRef(currentMockup))

  def toUIControl(uiProps: CanvasProps)(implicit currentMockup: Mockup, resolver: UIGenerator): Option[IUIControl]

  def markup: Boolean

// :( I don't know how to put it at MockupControl only
  def isValidDynamicFormControl: Boolean = false
}

trait ShapeTree {
  def current: RectangleShape
  def toMap: Map[RectangleShape, Seq[RectangleShape]] = getPairs.toMap
  protected[balsamiq] def getPairs: Seq[(RectangleShape, Seq[RectangleShape])]
  def filterMarkupElements: Either[ShapeTree, Seq[ShapeTree]]
  def print(prefix: String): Unit
}

case class ShapeTreeNode(val current: RectangleShape, val children: Seq[ShapeTree]) extends ShapeTree {
  override protected[balsamiq] def getPairs: Seq[(RectangleShape, Seq[RectangleShape])] = children.flatMap(_.getPairs).:+((current, children.map(_.current)))
  override def filterMarkupElements: Either[ShapeTree, Seq[ShapeTree]] = {
    val newChildren = children.flatMap(_.filterMarkupElements.fold(a => Seq(a), b => b))
    if (current.markup) Right(newChildren)
    else Left({
      if (newChildren.isEmpty) ShapeTreeList(current) else ShapeTreeNode(current, newChildren)
    })
  }

  override def print(prefix: String) = {
    println(s"${prefix}${toString}")
    children.foreach(_.print(prefix + "  "))
  }
}

case class ShapeTreeList(val current: RectangleShape) extends ShapeTree {
  override protected[balsamiq] def getPairs: Seq[(RectangleShape, Seq[RectangleShape])] = Seq((current, Seq()))
  override def filterMarkupElements: Either[ShapeTree, Seq[ShapeTree]] = {
    if (current.markup) Right(Seq()) else Left(this)
  }

  override def print(prefix: String) = println(s"${prefix}${toString}")
}


class MockupData(val fileName: String,
                 val mockupW: Int,
                 val mockupH: Int,
                 val measuredW: Int,
                 val measuredH: Int,
                 val controls: Seq[MockupControl])

trait Mockup extends RectangleShape with RectangleBySizeBase with CanvasLikeControlImpl {
  self: MockupData =>
  def fileName: String
  def width: Int = measuredW
  def height: Int = measuredH
  def controls: Seq[MockupControl]
  def x = controls.map(_.x).min
  def y = controls.map(_.y).min
  override def toString: String = s"${fileName} - width.height: ${width}.${height}"
  override def markup: Boolean = false
  def toUIControl(uiProps: CanvasProps)(implicit resolver: UIGenerator): Option[IUIControl] = super[CanvasLikeControlImpl].toUIControl(uiProps)(this, resolver)


  val dataSourceMap: Map[String, String] = controls.collect {
    case x: MockupDataSourceMapControl => x
  }.headOption.map(_.dataSourceMap) | Map()

  protected def arrangeShapesPossibleChildren(currentShape: RectangleShape, possibleChildren: Seq[RectangleShape]): (ShapeTree, Seq[RectangleShape]) = {
    val (children, notChildren) = possibleChildren.partition(_.isInside(currentShape))
    val firstOne = arrangeShapesKnownChildren(currentShape, children)
    (firstOne, notChildren)
  }

  protected def processShapesNotChildren(currentSet: Seq[ShapeTree], childrenToProcess: Seq[RectangleShape]): (Seq[ShapeTree], Seq[RectangleShape]) = {
    childrenToProcess match {
      case head :: headTail :: tail => {
        val (childNode, childrenToProcess) = arrangeShapesPossibleChildren(head, headTail :: tail)
        processShapesNotChildren(currentSet.:+(childNode), childrenToProcess)
      }
      case head :: Nil => (currentSet.:+(ShapeTreeList(head)), Nil)
      case Nil => (currentSet, Nil)
    }
  }

  protected def arrangeShapesKnownChildren(currentShape: RectangleShape, knownChildren: Seq[RectangleShape]): ShapeTree = {
    knownChildren match {
      case head :: tailHead :: tail => {
        val (children, emptyColl) = processShapesNotChildren(Seq(), knownChildren)
        ShapeTreeNode(currentShape, children)
      }
      case head :: Nil => ShapeTreeNode(currentShape, Seq(ShapeTreeList(head)))
      case Nil => ShapeTreeList(currentShape)
    }
  }

  val shapeTree: ShapeTree = arrangeShapesKnownChildren(this, controls.sortBy(_.square).reverse)
  val shapesMap: Map[RectangleShape, Seq[RectangleShape]] = shapeTree.toMap

// that's because markup is constantly false for mockup!
  val shapeTreeFiltered: ShapeTree = shapeTree.filterMarkupElements.left.get
  val shapesMapFiltered: Map[RectangleShape, Seq[RectangleShape]] = shapeTreeFiltered.toMap
}


object Mockup {
  def apply(caption: String, e: Node): Mockup = {
    val mockupW = (e \ "@mockupW").text.toInt
    val mockupH = (e \ "@mockupH").text.toInt
    val measuredW = (e \ "@measuredW").text.toInt
    val measuredH = (e \ "@measuredH").text.toInt
    val controls = (e \\ "control").map(MockupControl(_))
    new MockupData(caption, mockupW, mockupH, measuredW, measuredH, controls) with Mockup
  }
}

