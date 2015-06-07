package org.scaped

import java.io.{FileInputStream, InputStream}
import java.nio.ByteBuffer

package object parser {

    type Token = String

    val str = Iterator continually ()

    type BinaryFile = Iterator[Byte]
    def fromInput(is: InputStream): BinaryFile = {
        Iterator continually is.read takeWhile(-1 !=) map (_.toByte)
    }


    //case class BinaryFile(bytes: Array[Byte], var idx: Int)
    object ByteArrayPool {
        private val eight = 0 to 20 map (i => Array.ofDim[Byte](8))
        private val four = 0 to 20 map (i => Array.ofDim[Byte](4))
        private val twenty = 0 to 5 map (i => Array.ofDim[Byte](20))
        private var i4 = 0
        private var i8 = 0
        private var i20 = 0

        def nextFour = {
            val r = four(i4)
            if (i4 == 20) i4 = 0 else i4 += 1
            r
        }

        def nextEight = {
            val r = eight(i8)
            if (i8 == 20) i8 = 0 else i8 += 1
            r
        }

        def nextTwenty = {
            val r = twenty(i20)
            if (i20 == 5) i20 = 0 else i20 += 1
            r
        }

        def next(n: Int) =
            n match {
                case 4 => nextFour
                case 8 => nextEight
                case 20 => nextTwenty
            }

    }


    def field(n: Int) =
        (b: BinaryFile) => {
                val as = ByteArrayPool.next(n)
                0 until n foreach (i => as(i) = b.next())
                Some((as, b))
            }

    implicit def bytesToInt(b: Array[Byte]): Int = ByteBuffer.wrap(b).getInt
    implicit def bytesToDouble(b: Array[Byte]): Double = ByteBuffer.wrap(b).getDouble
}
