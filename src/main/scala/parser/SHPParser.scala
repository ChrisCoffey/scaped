package org.scaped.parser


import org.scaped._

case class ShpHeader(code: Int,
                     length: Int,
                     version: Int,
                     shapeType: Int,
                     mbr: MinimumBoundingRect,
                     minZ: ValueRange,
                     minM: ValueRange)
case class ValueRange(start: Double, end: Double)
case class MinimumBoundingRect(min: Point, max: Point)
case class Geometry()
case class ShpFile(header: ShpHeader, shapes: Seq[Shape])

sealed trait Shape
case object NullShape extends Shape
case class Part(len: Int) extends Shape
case class Point(x: Double, y: Double) extends Shape
case class PolyLine(mbr: MinimumBoundingRect, numParts: Int, numPoints: Int, parts: Seq[Part], points: Seq[Point]) extends Shape
case class Polygon(mbr: MinimumBoundingRect, numParts: Int, numPoints: Int, parts: Seq[Part], points: Seq[Point]) extends Shape
case class MultiPoint(mbr: MinimumBoundingRect, numPoints: Int, points: List[Point]) extends Shape
case class PointZ() extends Shape
case class PolylineZ() extends Shape
case class PolygonZ() extends Shape
case class MultiPointZ() extends Shape
case class PointM() extends Shape
case class PolyLineM() extends Shape
case class PolygonM() extends Shape
case class MultiPointM() extends Shape
case class MultiPatch() extends Shape


object SHPParser extends BinaryParser[ShpFile]{
    type R[T] = Resp[(T, BinaryFile)]
    def parse(b: BinaryFile): Resp[ShpFile] = {
        def go(a: BinaryFile, ls: Seq[Shape]): Seq[Shape] = {
            if(!a.hasNext) ls
            else {
                val (x, y) = extractEntry(a).getOrElse((NullShape, Iterator[Byte]()))
                go(y, ls :+ x )
            }
        }

        extractHeader(b).map(h => {
            val (header, bf) = h
            ShpFile(header, go(bf, Seq()))
        }).right
    }

    def extractEntry(b: BinaryFile): Option[(Shape, BinaryFile)] = {
        import SHPFormat.EntryHeader._
        import SHPFormat.Entry._

        for {
            (n, a) <- recordNumber(b)
            (len, c) <- length(a)
            (tpe, d) <- shapeType(c)
            _ = println(s"ShapeType: ${bytesToInt(tpe.reverse)}  with length ${bytesToInt(len)} and #${bytesToInt(n)}")
            r <- shape(tpe.reverse)(d)
        } yield r

    }

    def extractHeader(b: BinaryFile): Resp[(ShpHeader, BinaryFile)] = {
        import SHPFormat.Header._

        (for{
            (fc, b1) <- fileCode(b)
            (_, b2) <- empty(b1)
            (len, b3) <- fileLength(b2)
            (ver, b4) <- version(b3)
            (st, b5) <- shapeType(b4)
            (mnX, b6) <- mbrMinX(b5)
            (mnY, b7) <- mbrMinY(b6)
            (mxX, b8) <- mbrMaxX(b7)
            (mxY, b9) <- mbrMaxY(b8)
            (mnZ, b10) <- minZ(b9)
            (mxZ, b11) <- maxZ(b10)
            (mnM, b12) <- minM(b11)
            (mxM, b13) <- maxM(b12)
        } yield (ShpHeader(fc,
            len,
            ver.reverse,
            st.reverse,
            MinimumBoundingRect(Point(mnX.reverse, mnY.reverse), Point(mxX.reverse, mxY.reverse)),
            ValueRange(mnZ.reverse, mxZ.reverse),
            ValueRange(mnM.reverse, mxM.reverse)), b13)
        ).fold[R[ShpHeader]](Left("Invalid Header").right)(x => Right(x).right)
    }
}

object SHPFormat {
    type R[T] = Resp[(T, BinaryFile)]

    object Header {
        def fileCode = field(4)
        def empty = field(20)
        def fileLength = field(4)
        def version = field(4)
        def shapeType = field(4)
        def mbrMinX = field(8)
        def mbrMinY = field(8)
        def mbrMaxX= field(8)
        def mbrMaxY= field(8)
        def minZ = field(8)
        def maxZ= field(8)
        def minM = field(8)
        def maxM = field(8)
    }

    object EntryHeader{
        def recordNumber = field(4)
        def length = field(4)
    }

    object Entry {
        def shapeType = field(4)
        def shape(n: Int): BinaryFile => Option[(Shape, BinaryFile)] =
            n match {
                case 0 => Shapes.nullShape
                case 1 => Shapes.point
                case 3 => Shapes.polyLine
                case 5 => Shapes.polyGon
                case 8 => Shapes.multiPoint
                case _ => Shapes.nullShape
            }
    }

    object Shapes {
        type R[T <: Shape] = Option[(T, BinaryFile)]

        def nullShape(b: BinaryFile): R[NullShape.type] =
            Some(NullShape, b)

        def point(b: BinaryFile): R[Point] =
            for{
                (_, a) <- field(4)(b)
                (x, c) <- field(8)(a)
                (y, d) <- field(8)(c)
            } yield (Point(x.reverse, y.reverse), d)


        private def mbr(b: BinaryFile) =
            for{
                (mnX, a) <- field(8)(b)
                (mnY, c) <- field(8)(a)
                (mxX, d) <- field(8)(c)
                (mxY, e) <- field(8)(d)
            } yield (
                MinimumBoundingRect(Point(mnX.reverse, mnY.reverse), Point(mxX.reverse, mxY.reverse)),
                e)

        private def part(b: BinaryFile): R[Part]  =
            field(4)(b).map(x =>{
                (Part(x._1.reverse), x._2)
            } )


        private def shapeSeq[T <: Shape](n: Int,
                                         f: BinaryFile => Option[(T, BinaryFile)])
                                        (a: BinaryFile): Option[(Seq[T], BinaryFile)] = {
            def go(i: Int, bf: BinaryFile, ls: Seq[T]): Option[(Seq[T], BinaryFile)] = {
                i match {
                    case 0 => f(bf).map(x => (ls :+ x._1, x._2))
                    case _ => {
                        f(bf) match {
                            case Some(x) =>  go(i -1, x._2, ls :+ x._1)
                            case _ => None
                        }
                    }
                }
            }

            go(n, a, Seq[T]())

//            var tb = a
//            val x = (0 until n).map( i =>{
//                f(tb).map{ r => {
//                   tb = r._2
//                   r._1
//                }}
//            })
//            sequenceOpt(x).map(ls => (ls, tb))
        }

        def multiPoint(a: BinaryFile): R[MultiPoint] = {
            for{
                (_, b) <- field(4)(a)
                (mr, c) <- mbr(b)
                (rnumPts, d) <- field(4)(c)
                numPts = rnumPts.reverse
                (pts, e) <- shapeSeq(numPts, point)(d)
            } yield (
                MultiPoint(mr, numPts, pts.toList),
                e)
        }

        def polyLine(a: BinaryFile): R[PolyLine] =
            for{
                (_, b) <- field(4)(a)
                (box, c) <- mbr(b)
                (rnumParts, d) <- field(4)(c)
                nParts = rnumParts.reverse
                (rnPoints, e) <- field(4)(d)
                nPoints = rnPoints.reverse
                (prts, f) <- shapeSeq(nParts, part)(e)
                (pts, g) <- shapeSeq(nPoints, point)(f)
            } yield (
                PolyLine(box, nParts, nPoints, prts, pts),
                g)

        def polyGon(a: BinaryFile): R[Polygon] =
            for{
                (_, b) <- field(4)(a)
                (box, c) <- mbr(b)
                (rnumParts, d) <- field(4)(c)
                nParts = rnumParts.reverse
                (rnPoints, e) <- field(4)(d)
                nPoints = rnPoints.reverse
                (prts, f) <- shapeSeq(nParts, part)(e)
                (pts, g) <- shapeSeq(nPoints, point)(f)
            } yield ( Polygon(box, nParts, nPoints, prts, pts), g )

    }

}