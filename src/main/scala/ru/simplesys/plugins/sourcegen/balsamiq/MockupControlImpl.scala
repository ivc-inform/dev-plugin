package ru.simplesys.plugins
package sourcegen
package balsamiq

import scalaz.syntax.std.option._
import com.simplesys.common.equality.SimpleEquality._

//--DynamicFormLikeControl--------------------------------------------------------------------------------------------
trait DynamicFormLikeControlImpl {
  self: RectangleShape =>

  protected def findBeginnings(shapes: Seq[RectangleShape])(implicit direction: CurrentDirection): Seq[Int] = {
    import scala.collection.mutable.ListBuffer
    val groups = shapes.sortBy(_.start).foldLeft(Seq.empty[ListBuffer[RectangleShape]]) {
      (s, curr) =>
        val prev = s.lastOption
        prev match {
          case None => Seq(ListBuffer(curr))
          case Some(lb) => {
            if (curr.start < lb.last.end) {lb += curr; s}
            else s.:+(ListBuffer(curr))
          }
        }
    }

    val currRes = groups.map(_.map(_.start).min)

    val nestedRes = groups.flatten {gr =>
      val seq = gr.toSeq
      seq.sortBy(_.length)(Ordering[Int].reverse).drop(1) match {
          case Nil => Seq()
          case head :: Nil => Seq()
          case nonEmpty => {
            val res = findBeginnings(nonEmpty)
            // first column we need to exclude because it duplicates current one
            res.sortBy(x => x).drop(1)
          }
      }
    }
    currRes ++ nestedRes
  }

  protected def getRowColStart(shape: RectangleShape, rowsBeginning: Seq[Int], colsBeginning: Seq[Int]): (Int, Int) =
    (rowsBeginning.lastIndexWhere(start => shape.y >= start) + 1, colsBeginning.lastIndexWhere(start => shape.x >= start) + 1)

  protected def getRowColEnd(shape: RectangleShape, rowsBeginning: Seq[Int], colsBeginning: Seq[Int]): (Int, Int) =
    (rowsBeginning.lastIndexWhere(start => shape.bottom >= start) + 1, colsBeginning.lastIndexWhere(start => shape.right >= start) + 1)

  def toUIControl(uiProps: CanvasProps)(implicit currentMockup: Mockup, resolver: UIGenerator): Option[IUIControl] = {
    val childrenShapes = children

    val colsBeginning = findBeginnings(childrenShapes)(LeftToRightDirection).toIndexedSeq.sortBy(x => x)
    val rowsBeginning = findBeginnings(childrenShapes)(TopToBottomDirection).toIndexedSeq.sortBy(x => x)

    val bindedControls = childrenShapes.collect {case x: FormItemControl if (x.isBindedToAttr) => x}.map(x => (x, getRowColStart(x, rowsBeginning, colsBeginning), getRowColEnd(x, rowsBeginning, colsBeginning)))
    val bindedFields = bindedControls.sortBy {case (_, rowColStart, _) => rowColStart}.map {
      case (shape, (rowStart, colStart), (rowEnd, colEnd)) =>
        DynamicFormField(shape.bindedAttr.get,
                         if ((rowEnd - rowStart) > 0) Some(rowEnd - rowStart + 1) else None,
                         if ((colEnd - colStart) > 0) Some(colEnd - colStart + 1) else None,
                         None, None, rowStart, colStart)}

    new DynamicForm(dsLink, colsBeginning.size, rowsBeginning.size, bindedFields)(uiProps).some
  }
}
//--DynamicFormLikeControl-------------------------------------------------------------------------------------------


//--CanvasLikeControl------------------------------------------------------------------------------------------------
trait CanvasLikeControlImpl extends DynamicFormLikeControlImpl {
  self: RectangleShape =>

  override def toUIControl(uiProps: CanvasProps)(implicit currentMockup: Mockup, resolver: UIGenerator): Option[IUIControl] = {
    val childrenShape = children
    if (childrenShape.forall(_.isValidDynamicFormControl))
// we have only DynamicForm or empty canvas
      super[DynamicFormLikeControlImpl].toUIControl(uiProps)
    else {
// we don't have free DynamicFormItems or we have a mix

      val otherChildren = childrenShape.filterNot(_.isValidDynamicFormControl)

      childrenShape match {
// ook, we never reach this case
        case Nil => None
// ook, that's only one control and it's not a DynamicFormItem
        case head :: Nil => head.toUIControl(uiProps)
        case _ => {
// ook, let's check if we have DynamicFormItems and if they could produce only ONE DynamicForm rectangle
          val dynFormControls = childrenShape.filter(_.isValidDynamicFormControl)

          val dynForm: Option[RectangleShape] = dynFormControls match {
            case Nil => None
            case nel => {
              val newPossibleRect = resizeChildToPossibleBorders(RectangleByPoints(nel.map(_.x).min, nel.map(_.y).min, nel.map(_.right).max, nel.map(_.bottom).max), otherChildren)
              val fakeControl = new {
                val fakeChildren = nel
              } with RectangleByPoints(newPossibleRect.x, newPossibleRect.y, newPossibleRect.right, newPossibleRect.bottom) with FakeProvider with FakeControl with DynamicFormLikeControlImpl

              if (otherChildren.forall(!_.isOverlaps(fakeControl)))
                Some(fakeControl)
              else
                throw new RuntimeException(s"We have DynamicForm controls which could not be fitted to single DynamicForm control!!! (${currentMockup.toString}.${this.children.map(_.toString).mkString(",")})")
            }
          }
// ook, here we have only big enough controls, because we wrapped all possible DynamicFormItems into single DynamicForm
          val shapesToProcess = otherChildren ++ dynForm
          //detect layout type
          val xRight = shapesToProcess.map(curr => (curr, canChildResizeUpToXRight(curr, shapesToProcess.filter(_ !== curr))))
          val yBottom = shapesToProcess.map(curr => (curr, canChildResizeUpToYBottom(curr, shapesToProcess.filter(_ !== curr))))
          val xRightConcatenate = xRight.foldLeft(false)((sum, c) =>  {val (rect, isXRight) = c; sum || isXRight})
          val yBottomConcatenate = yBottom.foldLeft(false)((sum, c) =>  {val (rect, isYBottom) = c; sum || isYBottom})
          val (isVertical, groupedChildren) = (xRightConcatenate, yBottomConcatenate) match {
            case (true, true) => throw new RuntimeException(s"Can't decide between horizontal and vertical layout in ${currentMockup.toString}")
            case (false, false) => throw new RuntimeException(s"Can't determine type of layout, that's nor horizontal nor vertical in ${currentMockup.toString}")
            //xRight true, so we stack vertically using VLayout
            case (true, false) => (true, xRight.sortBy {case (rect, isXRight) => rect.y}.foldLeft(Seq.empty[(scala.collection.mutable.ListBuffer[RectangleShape], Boolean)]){case (seq, (rect, isXRight)) =>
                val prev = seq.lastOption
                prev match {
                  case Some(p @ (lseq, bval)) => if (bval === isXRight && !isXRight) {lseq += rect; seq} else seq.:+((scala.collection.mutable.ListBuffer(rect), isXRight))
                  case None => Seq((scala.collection.mutable.ListBuffer(rect), isXRight))
                }
              })
            //yBottom true, so we stack horizontally using HLayout
            case (false, true) => (false, yBottom.sortBy {case (rect, isYBottom) => rect.x}.foldLeft(Seq.empty[(scala.collection.mutable.ListBuffer[RectangleShape], Boolean)]){case (seq, (rect, isYBottom)) =>
                val prev = seq.lastOption
                prev match {
                  case Some(p @ (lseq, bval)) => if (bval === isYBottom && !isYBottom) {lseq += rect; seq} else seq.:+((scala.collection.mutable.ListBuffer(rect), isYBottom))
                  case None => Seq((scala.collection.mutable.ListBuffer(rect), isYBottom))
                }
              })
          }

          implicit val currDirection = if (isVertical) TopToBottomDirection else LeftToRightDirection

          val linearChildren = groupedChildren.flatMap {case (lb, canFit) => if (canFit) lb else Seq(
            new {
              val fakeChildren: Seq[RectangleShape] = lb
            } with RectangleByPoints(lb.map(_.x).min, lb.map(_.y).min, lb.map(_.right).max, lb.map(_.bottom).max) with FakeProvider with FakeControl with CanvasLikeControlImpl)
          }


          val childrenWOutSplitters = linearChildren.filter(x => !x.isInstanceOf[HSplitterControl] && !x.isInstanceOf[VSplitterControl])

          val overallLength = childrenWOutSplitters.map(_.length).sum

          val linearChildrenWSplitters = linearChildren.foldLeft(Seq.empty[(RectangleShape, Boolean, Option[String])]){
            (acc, curr) =>
              curr match {
                // if wrong splitter (or splitter is the first then just ignoring it
                case c: HSplitterControl => if (!isVertical && !acc.isEmpty) {
                  val (shape, splitter, target) = acc.last
                  acc.dropRight(1).:+((shape, true, None))
                } else acc
                case c: VSplitterControl => if (isVertical && !acc.isEmpty) {
                  val (shape, splitter, target) = acc.last
                  acc.dropRight(1).:+((shape, true, None))
                } else acc
                case c => if (acc.isEmpty) acc.:+((c, false, None))
                else {
                  val (shape, splitter, target) = acc.last
                  if (splitter) acc.dropRight(1) ++ Seq((shape, true, if (shape.length > c.length) Some("next") else None), (c, false, None))
                  else acc.:+((c, false, None))

                }
              }
          }

          val controls = linearChildrenWSplitters.flatten{
            case (x, splitter, target) =>
              x.toUIControl{
                if (isVertical)
                  CanvasProps(showResizeBar = {if (splitter) Some(true) else None}, resizeBarTarget = target,  widthPCT = 100.some, heightPCT = math.round(100*x.length/overallLength).some, direction = currDirection.some)
                else CanvasProps(showResizeBar = {if (splitter) Some(true) else None}, resizeBarTarget = target, widthPCT = math.round(100*x.length/overallLength).some, heightPCT = 100.some, direction = currDirection.some)
              }
          }

          val layout = if (isVertical) new VLayout(dsLink, None, Some(controls))(uiProps) else new HLayout(dsLink, None, Some(controls))(uiProps)
          Some(layout)
        }
      }
    }
  }
}
//--CanvasLikeControl------------------------------------------------------------------------------------------------

//--NonUILikeControl-------------------------------------------------------------------------------------------------
trait NonUILikeControlImpl {
  self: RectangleShape =>
  def toUIControl(uiProps: CanvasProps)(implicit currentMockup: Mockup, resolver: UIGenerator): Option[IUIControl] = None
}
//--NonUILikeControl-------------------------------------------------------------------------------------------------