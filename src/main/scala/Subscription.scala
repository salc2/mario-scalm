import org.scalajs.dom
import org.scalajs.dom.KeyboardEvent
import scalm.Sub
import scalm.Sub.ofTotalObservable

object Subscription {

  val requestAnimationFrameSub:Sub[Double] = ofTotalObservable[Double](
    "requestAnimation", { observer =>
      var handle = 0
      def loop(time: Double): Unit = {
        observer.onNext(time)
        handle = dom.window.requestAnimationFrame(loop)
      }
      handle = dom.window.requestAnimationFrame(loop)
      () =>
        dom.window.cancelAnimationFrame(handle)
    }
  )

  val keyPressSubscriber:Sub[Int] = ofTotalObservable[Int](
    "keyPress", { observer =>
      dom.window.onkeypress = (keyEvent: KeyboardEvent) => {
        observer.onNext(keyEvent.keyCode)
      }
      () =>
        dom.window.onkeypress = null
    }
  )

}
