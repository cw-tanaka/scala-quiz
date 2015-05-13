package com.chatwork.quiz.collection

import com.chatwork.quiz.{MyNone, MySome, MyOption}

sealed trait MyList[+A] {

  // Easy
  def length: Int = this match {
    case MyCons(h, t) => 1 + t.length
    case MyNil => 0
  }

  // Normal
  def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
    case MyCons(head, tail) => tail.foldLeft(f(z, head))(f)
    case MyNil => z
  }

  // 難易度選択制
  // Normal: 条件 - 特にありません、気の向くままに実装してください。
  // Hard:   条件 - foldLeftを使って実装してください。
  def foldRight[B](z: B)(f: (A, B) => B): B = this match {
    case MyCons(head, tail) => f(head, tail.foldRight(z)(f))
    case MyNil => z
  }

  // Normal
  // scalastyle:off
  def ::[B >: A](b: B): MyList[B] = MyCons(b, this)
  // scalastyle:on

  // Normal
  def reverse: MyList[A] = foldLeft[MyList[A]](MyNil)((acc, x) => MyCons(x, acc))

  // Normal
  // scalastyle:off
  def ++[B >: A](b: MyList[B]): MyList[B] = ???
  // scalastyle:on

  // Normal
  def map[B](f: A => B): MyList[B] = this match {
    case MyCons(head, tail) => MyCons(f(head), tail.map(f))
    case _ => MyNil
  }

  // Normal
  def flatMap[B](f: A => MyList[B]): MyList[B] = ???

  // Normal
  def filter(f: A => Boolean): MyList[A] = this match {
    case MyCons(head, tail) => if (f(head)) MyCons(head, tail.filter(f)) else tail.filter(f)
    case _ => MyNil
  }

  // Normal: 条件 - filterと同様の実装でも構いません。
  // Hard:   条件 - 中間リストを生成しないように実装してください。
  def withFilter(f: A => Boolean): MyList[A] = ???

  // Normal
  def find(f: A => Boolean): MyOption[A] = this match {
    case MyCons(head, tail) => if (f(head)) MySome(head) else tail.find(f)
    case _ => MyNone
  }

  // Normal
  def startsWith[B >: A](prefix: MyList[B]): Boolean = (this, prefix) match {
    case (MyCons(h1, t1), MyCons(h2, t2)) => if (h1 == h2) t1.startsWith(t2) else false
    case _ => true
  }

}

case object MyNil extends MyList[Nothing]

case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  // Easy
  def empty[A]: MyList[A] = MyNil

  // Normal
  def apply[A](as: A*): MyList[A] = as.foldRight[MyList[A]](MyNil)((x, acc) => MyCons(x, acc))
}
