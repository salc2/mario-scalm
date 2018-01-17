import Main.{Keys, Msg, PassageOfTime}
import org.scalajs.dom
import org.scalajs.dom.KeyboardEvent
import scalm.Sub.ofTotalObservable

object MoreSubscriber {

  val subFps = ofTotalObservable[Msg](
    "fps", { observer =>
      var handle = 0
      def loop(time: Double): Unit = {
        observer.onNext(PassageOfTime(time / 10000))
        handle = dom.window.requestAnimationFrame(loop)
      }
      handle = dom.window.requestAnimationFrame(loop)
      () =>
        dom.window.cancelAnimationFrame(handle)
    }
  )

  val subKeys = ofTotalObservable[Msg](
    "keys", { observer =>
      dom.window.onkeydown = (keyEvent: KeyboardEvent) => {
        keyEvent.keyCode match {
          case 37 => observer.onNext(Keys(-1, 0))
          case 38 => observer.onNext(Keys(0, +1))
          case 39 => observer.onNext(Keys(+1, 0))
          case _  => ()
        }
      }
      dom.window.onkeyup = (keyEvent: KeyboardEvent) => {
        keyEvent.keyCode match {
          case 37 => observer.onNext(Keys(0, 0))
          case 38 => observer.onNext(Keys(0, 0))
          case 39 => observer.onNext(Keys(0, 0))
          case _  => ()
        }
      }
      () =>
        dom.window.onkeypress = null
        dom.window.onkeyup = null
    }
  )

}
