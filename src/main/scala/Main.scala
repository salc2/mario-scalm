import org.scalajs.dom.document
import scalm.Html._
import scalm._
import MoreHTML._

import scala.math.max

object Main extends App {

  def main(args: Array[String]): Unit = Scalm.start(this, document.body)

  sealed trait Direction
  case object Left extends Direction
  case object Right extends Direction

  case class Mario(x: Double, y: Double, vx: Double, vy: Double, dir: Direction)

  type Model = Mario

  def init: (Model, Cmd[Msg]) = (Mario(0, 0, 0, 0, Right), Cmd.Empty)

  sealed trait Msg
  case class PassageOfTime(time: Double) extends Msg
  case class Keys(x: Int, y: Int) extends Msg

  def gravity(mario: Model): Model =
    mario.copy(vy = if (mario.y > 0) mario.vy - 0.4 else 0)

  def physics(mario: Model): Model =
    mario.copy(x = mario.x + mario.vx, y = max(0.0, mario.y + 3 * mario.vy))

  def walk(keys: Keys)(mario: Model): Model =
    mario.copy(vx = keys.x,
               dir =
                 if (keys.x < 0) Left else if (keys.x > 0) Right else mario.dir)

  def jump(keys: Keys)(mario: Model): Model =
    if (keys.y > 0 && mario.vy == 0) mario.copy(vy = 6.0) else mario

  def step(keys: Keys, model: Model): Model = {
    val m = gravity(model)
    val m1 = jump(keys)(m)
    val m2 = walk(keys)(m1)
    physics(m2)
  }

  def step(model: Model): Model = {
    val m = gravity(model)
    physics(m)
  }

  def update(msg: Msg, model: Model): (Model, Cmd[Msg]) =
    msg match {
      case key: Keys        => (step(key, model), Cmd.Empty)
      case PassageOfTime(_) => (step(model), Cmd.Empty)
    }

  def subscriptions(model: Model): Sub[Msg] = {
    Sub.Combine(MoreSubscriber.subKeys, MoreSubscriber.subFps)
  }

  def view(model: Model): Html[Msg] = {

    val verb = (model.y > 0, model.vx != 0) match {
      case (true, _) => "jump"
      case (_, true) => "walk"
      case _         => "stand"
    }

    val dir = model.dir.toString.toLowerCase
    val transform = s"transform: matrix(1, 0, 0, 1, ${277 + model.x}, ${250.5 - model.y})"
    val css =
      s"padding: 0px; margin: 0px; display: block; width: 35px; height: 35px; position: absolute; opacity: 1; $transform; background-color: transparent;"

    div()(
      img(style(css), src(s"/images/$verb/$dir.gif")),
      text(model.toString)
    )
  }

}
