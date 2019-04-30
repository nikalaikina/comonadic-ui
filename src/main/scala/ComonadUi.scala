import cats.Comonad
import cats.data.State
import cats.syntax.coflatMap._
import cats.syntax.comonad._

object ComonadUi extends App {
  import State._
  import Store._

  val w = Store(identity[Int], 0)
  val actions = for {
    _ <- set(5)
    _ <- modify[Int](_ + 5)
    s <- get
    r <- set(s * 3 + 1)
  } yield r

  val w2 = pairing.select(actions)(w.coflatten)
  println(w2.extract)
}

case class Store[S, A](show: S => A, state: S)

object Store {
  implicit def comonad[S]: Comonad[Store[S, ?]] = new Comonad[Store[S, ?]] {
    override def extract[A](x: Store[S, A]): A = x.show(x.state)

    override def coflatMap[A, B](fa: Store[S, A])(f: Store[S, A] => B): Store[S, B] = {
      Store(s => f(Store(fa.show, s)), fa.state)
    }

    override def map[A, B](fa: Store[S, A])(f: A => B): Store[S, B] = {
      Store(fa.show andThen f, fa.state)
    }
  }

  implicit def pairing[S] = new Pairing[State[S, ?], Store[S, ?]] {
    override def pair[A, B, C]: (A => B => C) => State[S, A] => Store[S, B] => C = { f => state => store =>
      val (next, a) = state.run(store.state).value
      f(a)(store.show(next))
    }
  }
}

trait Pairing[M[_], W[_]] {
  def pair[A, B, C]: (A => B => C) => M[A] => W[B] => C
  def select[A, B]: M[B] => W[W[A]] => W[A] = pair(_ => identity)
}
