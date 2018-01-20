package mario

import org.scalajs.dom.document
import scalm.Html._
import scalm.{Task, _}
import org.scalajs.dom.raw.HTMLAudioElement
import scala.math._
import org.scalajs.dom.raw.Event
import Html._

object Main extends App {

  def main(args: Array[String]): Unit =
    Scalm.start(this, document.querySelector("#mario"))

  sealed trait Direction
  case object Left extends Direction
  case object Right extends Direction

  case class Mario(x: Double, y: Double, vx: Double, vy: Double, dir: Direction)

  type Model = Mario

  def init: (Model, Cmd[Msg]) = (Mario(0, 0, 0, 0, Right), Cmd.Empty)

  sealed trait Msg
  case object PassageOfTime extends Msg
  case object Void extends Msg
  case object ArrowLeft extends Msg
  case object ArrowUp extends Msg
  case object ArrowRight extends Msg

  val friction = 0.025
  val gravity = 0.25

  val applyGravity: Mario => Mario = (mario) =>
    mario.copy(vy = if (mario.y > 0) mario.vy - gravity else 0)

  val applyMotion: Mario => Mario = (mario: Model) =>
    mario.copy(x = mario.x + mario.vx, y = max(0.0, mario.y + 3 * mario.vy))

  val walkLeft: Model => Model = _.copy(vx = -1.5, dir = Left)

  val walkRight: Model => Model = _.copy(vx = 1.5, dir = Right)

  val jump: Model => Model = _.copy(vy = 6.0)

  val applyFriction: Mario => Mario = (model: Model) =>
    if (model.y > 0) model
    else if (model.vx == 0.0) model
    else if (abs(model.vx) <= friction) model.copy(vx = 0.0)
    else if (model.vx > 0.0) model.copy(vx = model.vx - friction)
    else model.copy(vx = model.vx + friction)

  val applyPhysics
    : Model => Model = applyGravity compose applyMotion compose applyFriction

  def update(msg: Msg, model: Model): (Model, Cmd[Msg]) =
    msg match {
      case ArrowUp       => ((jump andThen applyPhysics)(model), Effects.Cmd.playSound("resources/jump-c-07.mp3",Void))
      case ArrowLeft     => ((walkLeft andThen applyPhysics)(model), Cmd.Empty)
      case ArrowRight    => ((walkRight andThen applyPhysics)(model), Cmd.Empty)
      case PassageOfTime => (applyPhysics(model), Cmd.Empty)
      case Void          => (model, Cmd.Empty)
    }

  def subscriptions(model: Model): Sub[Msg] = {
    val keyLeftSub = Effects.keyPressSub(37, ArrowLeft)
    val keyUpSub = Effects.keyPressSub(38, ArrowUp)
    val keyRightSub = Effects.keyPressSub(39, ArrowRight)
    val fpsSub = Effects.requestAnimationFrameSub.map(_ => PassageOfTime)

    Sub.Combine(fpsSub, keyLeftSub).combine(keyUpSub).combine(keyRightSub)
  }

  def view(model: Model): Html[Msg] = {

    val verb = (model.y > 0, model.vx != 0) match {
      case (true, _) => "jump"
      case (_, true) => "walk"
      case _         => "stand"
    }

    val dir = model.dir.toString.toLowerCase
    val transform =
      s"transform: matrix(1, 0, 0, 1, ${277 + model.x}, ${489.5 - model.y})"
    val css =
      s"padding: 0px; margin: 0px; display: block; width: 35px; height: 35px; position: absolute; opacity: 1; $transform; background-color: transparent;"

    div()(
      img(style(css), src(s"/resources/mario/$verb/$dir.gif"))
    )
  }

}
