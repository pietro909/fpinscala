package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) =>
        1 + size(left) + size(right)
    }

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(value) => value
      case Branch(left, right) =>
        maximum(left) max maximum(right)
    }

  def depth[A](tree: Tree[A]): Int = {
    def loop(acc: Int, branch: Tree[A]): Int =
      branch match {
        case Leaf(_) => acc
        case Branch(left, right) =>
          loop(acc + 1, left) max loop(acc + 1, right)
      }

    loop(1, tree)
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) =>
        Branch(map(left)(f), map(right)(f))
    }

  def fold[A, B](t: Tree[A])(l: A => B)(b: (B,B) => B): B =
    t match {
      case Leaf(value) => l(value)
      case Branch(left, right) =>
        b(fold(left)(l)(b), fold(right)(l)(b))
    }

  def fSize[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)((a, b) => a + b + 1)

  def fMaximum(tree: Tree[Int]): Int =
    fold[Int, Int](tree)(a => a)((a, b) => a max b)

  def fDepth[A](tree: Tree[A]): Int = {
    def loop(acc: Int, branch: Tree[A]): Int =
      fold(branch)(_ => acc)((accL, accR) => 1 + (accL max accR))
    loop(1, tree)
  }

  def fMap[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(v => Leaf(f(v)): Tree[B])((a, b) => Branch(a, b))

}
