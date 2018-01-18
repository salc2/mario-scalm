import org.scalajs.dom.document
import scalm.Html._
import scalm._
import Html._

import scala.math._

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
  trait Input extends Msg
  case object ArrowLeft extends Input
  case object ArrowUp extends Input
  case object ArrowRight extends Input
  case class OtherKey(code: Int) extends Input

  def gravity(mario: Model): Model =
    mario.copy(vy = if (mario.y > 0) mario.vy - 0.25 else 0)

  def physics(mario: Model): Model =
    mario.copy(x = mario.x + mario.vx, y = max(0.0, mario.y + 3 * mario.vy))

  def walk(input: Input)(mario: Model): Model =
    input match {
      case ArrowLeft  => mario.copy(vx = -1, dir = Left)
      case ArrowRight => mario.copy(vx = 1, dir = Right)
      case _          => mario
    }

  def jump(input: Input)(mario: Model): Model =
    input match {
      case ArrowUp if mario.vy == 0 => mario.copy(vy = 6.0)
      case _                        => mario
    }

  def step(input: Input, model: Model): Model = {
    val m = gravity(model)
    val m1 = applyFriction(m)
    val m2 = jump(input)(m1)
    val m3 = walk(input)(m2)
    physics(m3)
  }

  def applyFriction(model: Model): Model = {
    if(model.y > 0) model
    else if (model.vx == 0.0) model
    else if (abs(model.vx) <= 0.01) model.copy(vx = 0.0)
    else if (model.vx > 0.0) model.copy(vx = model.vx - 0.01)
    else model.copy(vx = model.vx + 0.01)
  }

  def step(model: Model): Model = {
    val m = gravity(model)
    val m1 = applyFriction(m)
    physics(m1)
  }

  def update(msg: Msg, model: Model): (Model, Cmd[Msg]) =
    msg match {
      case input: Input  => (step(input, model), Cmd.Empty)
      case PassageOfTime => (step(model), Cmd.Empty)
    }

  def subscriptions(model: Model): Sub[Msg] = {
    val keySub = Subscription.keyPressSubscriber.map {
      case 37   => ArrowLeft
      case 38   => ArrowUp
      case 39   => ArrowRight
      case code => OtherKey(code)
    }
    val fpsSub = Subscription.requestAnimationFrameSub.map(_ => PassageOfTime)
    Sub.Combine(keySub, fpsSub)
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
