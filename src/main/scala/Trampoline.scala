sealed trait Trampoline[+A] {
  def run: A = {
    @annotation.tailrec
    def loop(t: Trampoline[A]): A = t match {
      case Trampoline.Done(result) => result
      case Trampoline.More(thunk) => loop(thunk())
    }
    loop(this)
  }

  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = this match {
    case Trampoline.Done(result) => f(result)
    case Trampoline.More(thunk) => Trampoline.More(() => thunk().flatMap(f))
  }

  def map[B](f: A => B): Trampoline[B] = flatMap(a => Trampoline.Done(f(a)))
}

object Trampoline {
  case class Done[+A](result: A) extends Trampoline[A]
  case class More[+A](thunk: () => Trampoline[A]) extends Trampoline[A]

  def done[A](result: A): Trampoline[A] = Done(result)
  def more[A](thunk: => Trampoline[A]): Trampoline[A] = More(() => thunk)
}

object TrampolineExample {
  import Trampoline._

  def even(i: Int): Trampoline[Boolean] = i match {
    case 0 => done(true)
    case _ => more(odd(i - 1))
  }

  def odd(i: Int): Trampoline[Boolean] = i match {
    case 0 => done(false)
    case _ => more(even(i - 1))
  }
}