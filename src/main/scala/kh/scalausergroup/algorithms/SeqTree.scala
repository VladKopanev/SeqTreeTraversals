package kh.scalausergroup.algorithms

import scala.annotation.tailrec

case class SeqTree[A](a: A, sub: Seq[SeqTree[A]]) {

  def apply(i: Int): SeqTree[A] = applyTailRec(i)

  //naive folding
  def applyWithList(i: Int): SeqTree[A] = {
    def loop(treeSeq: Seq[SeqTree[A]], acc: List[SeqTree[A]]): List[SeqTree[A]] = treeSeq match {
      case Nil => acc
      case head :: tail => head :: loop(head.sub, loop(tail, List.empty) ::: acc)
    }
    loop(sub, List.empty)(i)
  }

  //just a copy of applyList but with Vector as accumulator type
  def applyWithVector(i: Int): SeqTree[A] = {
    def loop(treeSeq: Seq[SeqTree[A]], acc: Vector[SeqTree[A]]): Vector[SeqTree[A]] = treeSeq match {
      case Nil => acc
      case head :: tail => head +: loop(head.sub, loop(tail, Vector.empty) ++: acc)
    }
    loop(sub, Vector.empty)(i)
  }

  //just a copy of applyList but with Stream as accumulator type
  def applyWithStream(i: Int): SeqTree[A] = {
    def loop(treeSeq: Seq[SeqTree[A]], acc: Stream[SeqTree[A]]): Stream[SeqTree[A]] = treeSeq match {
      case Nil => acc
      case head :: tail => head #:: loop(head.sub, loop(tail, Stream.empty) #::: acc)
    }
    loop(sub, Stream.empty)(i)
  }

  def applyFold(i: Int): SeqTree[A] = {
    def fold(start: Seq[SeqTree[A]]): Seq[SeqTree[A]] =
      start.foldRight(Seq.empty[SeqTree[A]])((tree, acc) => tree +: (fold(tree.sub) ++ acc))
    fold(sub)(i)
  }

  def applyTailRec(i: Int): SeqTree[A] = {
    @tailrec
    def loop(treeSeq: Seq[SeqTree[A]], index: Int, continue: Seq[SeqTree[A]]): Option[SeqTree[A]] = treeSeq match {
      case Seq(head, _*) if index == 0 => Some(head)
      case Seq(head, tail@_*) => loop(head.sub, index - 1, tail ++ continue)
      case _  if index >= 0 && continue.nonEmpty => loop(continue, index, Seq.empty)
      case _  => None
    }
    loop(Seq(this), i, Seq.empty).getOrElse(throw new IndexOutOfBoundsException(s"index: $i"))
  }

}
