package org

import scala.util.Either.RightProjection

package object scaped {

    type Err[T] = Either[String, T]
    type Resp[T] = RightProjection[String, T]
    implicit def asResp[T](e: Err[T]): Resp[T] = e.right

    implicit def lAsResp[T](l: Left[String, T]): Resp[T] = l.right
    implicit def rAsResp[T](r: Right[String, T]): Resp[T] = r.right



    def sequenceOpt[A](ls: Seq[Option[A]]): Option[Seq[A]] =
        ls.foldRight(Option(Seq[A]()))((o, acc) =>{
            o match {
                case Some(x) => acc.map(s => x +: s)
                case None => None
            }
        })

}

