import cats.data.State
import cats.syntax.coflatMap._
import cats.syntax.comonad._
import cats.syntax.functor._
import cats.{Comonad, Functor, Monad}

import scala.language.higherKinds

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

object Util {
  def pairTransition[W[_]: Functor, A, B, C](f: A => B => C)(t: Transition[W, B])(w: W[A]): C = {
    t.runTransition(w.map(f))
  }

  def moveLeft: Transition[Zipper, Unit] = new Transition[Zipper, Unit] {
    override def runTransition[R]: Zipper[Unit => R] => R = { z: Zipper[Unit => R] =>
      z.left().get.extract(())
    }
  }

  def moveRight: Transition[Zipper, Unit] = new Transition[Zipper, Unit] {
    override def runTransition[R]: Zipper[Unit => R] => R = { z: Zipper[Unit => R] =>
      z.right().get.extract(())
    }
  }

  def get[S]: Transition[Store[S, ?], S] = new Transition[Store[S, ?], S] {
    override def runTransition[R]: Store[S, S => R] => R = { s: Store[S, S => R] =>
      s.extract(s.state)
    }
  }

  def put[S](state: S): Transition[Store[S, ?], Unit] = new Transition[Store[S, ?], Unit] {
    override def runTransition[R]: Store[S, Unit => R] => R = { s =>
      s.show(state)()
    }
  }
}

case class Zipper[A](l: List[A], x: A, r: List[A]) {
  def left(): Option[Zipper[A]] = l match {
    case head :: xs => Some(Zipper(xs, head, x :: r))
    case _ => None
  }

  def leftList(): List[Zipper[A]] = left().fold(List.empty[Zipper[A]])(z => z :: z.leftList())

  def right(): Option[Zipper[A]] = r match {
    case head :: xs => Some(Zipper(x :: l, head, xs))
    case _ => None
  }

  def rightList(): List[Zipper[A]] = right().fold(List.empty[Zipper[A]])(z => z :: z.rightList())
}

case object Zipper {
  implicit val comonad: Comonad[Zipper] = new Comonad[Zipper] {
    override def extract[A](x: Zipper[A]): A = x.x

    override def coflatMap[A, B](fa: Zipper[A])(f: Zipper[A] => B): Zipper[B] = {
      Zipper(fa.leftList().map(f), f(fa), fa.rightList().map(f))
    }

    override def map[A, B](fa: Zipper[A])(f: A => B): Zipper[B] = Zipper(fa.l.map(f), f(fa.x), fa.r.map(f))
  }
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

  implicit def pairing[S]: Pairing[State[S, ?], Store[S, ?]] = new Pairing[State[S, ?], Store[S, ?]] {
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

trait Transition[W[_], A] {
  def runTransition[R]: W[A => R] => R
}

object Transition {

  def extend[W[_]: Comonad : Functor, A, B]: (W[A] => B) => W[A] => W[B] = { f => _.coflatten.map(f) }

  implicit def functor[W[_]: Functor]: Functor[Transition[W, ?]] = new Functor[Transition[W, ?]] {
    override def map[A, B](fa: Transition[W, A])(f: A => B): Transition[W, B] = {
      new Transition[W, B] {
        def runTransition[R]: W[B => R] => R = { wbr => fa.runTransition(wbr.map(f andThen _)) }
      }
    }
  }

  implicit def monad[W[_]: Comonad: Functor, R]: Monad[Transition[W, ?]] = new Monad[Transition[W, ?]] {
    override def flatMap[A, B](fa: Transition[W, A])(f: A => Transition[W, B]): Transition[W, B] = new Transition[W, B] {
      override def runTransition[R]: W[B => R] => R = { wbr: W[B => R] =>
        val x: W[B => R] => A => R = { wa: W[B => R] => a: A => f(a).runTransition(wa) }
        fa.runTransition(wbr.coflatMap(x))
      }
    }

    override def pure[A](x: A): Transition[W, A] = new Transition[W, A] {
      override def runTransition[R]: W[A => R] => R = _.extract(x)
    }

    override def tailRecM[A, B](a: A)(f: A => Transition[W, Either[A, B]]): Transition[W, B] = ???
  }

}
