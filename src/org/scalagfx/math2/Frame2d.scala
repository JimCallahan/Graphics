// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math2

import scala.math.{ abs, sin, cos }
import java.nio.{ FloatBuffer, DoubleBuffer }

//--------------------------------------------------------------------------------------------------
//            Virtual Matrix (i, j)                                                         
//                                                                                          
//   | basisX.x basisY.x origin.x |                                                
//   | basisX.y basisY.y origin.y |                                                
//   | 0.0      0.0      1.0      |                                                
//--------------------------------------------------------------------------------------------------

class Frame2d private (val basisX: Vec2d, val basisY: Vec2d, val origin: Pos2d)
  extends Frame2[Double, Vec2d, Pos2d]
  with FrameOps[Double, Vec2d, Pos2d, Frame2d] {

  /** Compares this position to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Frame2d =>
        (that canEqual this) &&
          (basisX == that.basisX) && (basisY == that.basisY) && (origin == that.origin)
      case _ => false
    }

  def canEqual(that: Any): Boolean =
    that.isInstanceOf[Frame2d]

  /** Returns a hash code value for the object. */
  override def hashCode: Int =
    47 * (43 * (41 + basisX.hashCode) + basisY.hashCode) + origin.hashCode

  def apply(i: Int, j: Int) =
    try {
      i match {
        case 0 | 1 =>
          j match {
            case 0 => basisX(i)
            case 1 => basisY(i)
            case 2 => origin(i)
            case _ => throw new IllegalArgumentException
          }
        case 2 =>
          j match {
            case 0 | 1 => 0.0
            case 2     => 1.0
            case _     => throw new IllegalArgumentException
          }
        case _ => throw new IllegalArgumentException
      }
    }
    catch {
      case _: IllegalArgumentException =>
        throw new IllegalArgumentException("Invalid index (" + i + ", " + j + ")!")
    }

  def xform(p: Pos2d) = origin + basisX * p.x + basisY * p.y
  def *(p: Pos2d) = xform(p)

  def xform(v: Vec2d) = basisX * v.x + basisY * v.y
  def *(v: Vec2d) = xform(v)

  def concat(that: Frame2d) = Frame2d(xform(that.basisX), xform(that.basisY), xform(that.origin))
  def *(that: Frame2d) = concat(that)

  def inverse(): Option[Frame2d] = {

    /* lookup the column vector with the given index */
    def getColumn(fr: Frame2d, j: Int) =
      j match {
        case 0 => fr.basisX
        case 1 => fr.basisY
      }

    /* compute the inverse of the basis 2x2 matrix */
    val inv22 = {
      def f(act: Int, cs: Frame2d, inv: Frame2d): Option[(Frame2d, Frame2d)] =
        act match {
          case 2 => Some(cs, inv)
          case _ => {
            /* find largest pivot value and index in active column at or below active row */
            val (pivot, pi) = {
              def g(i: Int, p: Double, pi: Int, ls: List[Double]): (Double, Int) =
                ls match {
                  case d :: ds =>
                    if (abs(d) > abs(p)) g(i + 1, d, act + i, ds)
                    else g(i + 1, p, pi, ds)
                  case Nil => (p, pi)
                }

              g(0, 0.0, act, getColumn(cs, act).toList.drop(act))
            }

            /* if the pivot is zero (or nearly so), it can't be inverted! */
            if (Scalar.equiv(pivot, 0.0))
              None
            else {
              /* swap the pivot row with the active row (if they are different) */
              val (cs2, inv2) =
                if (pi > act) (cs.rowOpI(pi, act), inv.rowOpI(pi, act))
                else (cs, inv)

              /* normalize the active row by multiplying it by 1/pivot */
              val (cs3, inv3) =
                (cs2.rowOpII(act, 1.0 / pivot), inv2.rowOpII(act, 1.0 / pivot))

              /* subtract the proper multiple of the active row from each of the other rows
                 so that they have zero's in the pivot column */
              val (cs4, inv4) = {
                def g(i: Int, cs5: Frame2d, inv5: Frame2d): (Frame2d, Frame2d) =
                  i match {
                    case 2 => (cs5, inv5)
                    case _ if (i != act) =>
                      val scale = getColumn(cs5, act)(i) * -1.0
                      if (!Scalar.equiv(scale, 0.0))
                        g(i + 1, cs5.rowOpIII(i, act, scale), inv5.rowOpIII(i, act, scale))
                      else
                        g(i + 1, cs5, inv5)
                    case _ => g(i + 1, cs5, inv5)
                  }

                g(0, cs3, inv3)
              }

              /* process next row... */
              f(act + 1, cs4, inv4)
            }
          }
        }

      f(0, this, Frame2d())
    }

    /* handle the last (virtual) row:							  
        rowOpI can be skipped since this is the last row.
        rowOpII can be skipped since the pivot is 1 by definition.
        rowOpIII can be simplified since the last row is all zeros except 		  
          for the last column which is one -- this means that only the last 		  
	  column of the other rows need to be altered. */
    inv22 match {
      case Some(Pair(cs, inv)) =>
        Some(Frame2d(inv.basisX, inv.basisY, (inv.origin - cs.origin).toPos2d))
      case None => None
    }
  }

  def rowOpI(i1: Int, i2: Int) =
    if (i1 == i2) this
    else Frame2d(basisX.update(i1, basisX(i2)).update(i2, basisX(i1)),
      basisY.update(i1, basisY(i2)).update(i2, basisY(i1)),
      origin.update(i1, origin(i2)).update(i2, origin(i1)))

  def rowOpII(i: Int, scale: Double) =
    Frame2d(basisX.update(i, basisX(i) * scale),
      basisY.update(i, basisY(i) * scale),
      origin.update(i, origin(i) * scale))

  def rowOpIII(i1: Int, i2: Int, scale: Double) =
    Frame2d(basisX.update(i1, basisX(i1) + basisX(i2) * scale),
      basisY.update(i1, basisY(i1) + basisY(i2) * scale),
      origin.update(i1, origin(i1) + origin(i2) * scale))

  def forall(p: (Double) => Boolean): Boolean =
    basisX.forall(p) && basisY.forall(p) && origin.forall(p)

  def forall(that: Frame2d)(p: (Double, Double) => Boolean): Boolean =
    basisX.forall(that.basisX)(p) && basisY.forall(that.basisY)(p) &&
      origin.forall(that.origin)(p)

  def equiv(that: Frame2d, epsilon: Double): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))
  def equiv(that: Frame2d): Boolean = forall(that)(Scalar.equiv(_, _))

  def forany(p: (Double) => Boolean): Boolean =
    basisX.forany(p) && basisY.forany(p) && origin.forany(p)

  def forany(that: Frame2d)(p: (Double, Double) => Boolean): Boolean =
    basisX.forany(that.basisX)(p) && basisY.forany(that.basisY)(p) &&
      origin.forany(that.origin)(p)

  def foreach(f: (Double) => Unit): Unit = {
    basisX.foreach(f); basisY.foreach(f); origin.foreach(f)
  }

  def map(f: (Double) => Double): Frame2d =
    Frame2d(basisX.map(f), basisY.map(f), origin.map(f))

  /** Convert to a string representation. */
  override def toString() = "Frame2d(" + basisX + ", " + basisY + ", " + origin + ")"

  def toList: List[List[Double]] =
    List(List(basisX.x, basisX.y, 0.0),
      List(basisY.x, basisY.y, 0.0),
      List(origin.x, origin.y, 1.0))

  def toArray: Array[Array[Double]] =
    Array(Array(basisX.x, basisX.y, 0.0),
      Array(basisY.x, basisY.y, 0.0),
      Array(origin.x, origin.y, 1.0))

  def putNative(buf: DoubleBuffer) {
    buf.put(basisX.x); buf.put(basisX.y); buf.put(0.0)
    buf.put(basisY.x); buf.put(basisY.y); buf.put(0.0)
    buf.put(origin.x); buf.put(origin.y); buf.put(1.0)
  }
  def >>>(buf: FloatBuffer) { putNative(buf) }

  def putNative(buf: FloatBuffer) {
    buf.put(basisX.x.toFloat); buf.put(basisX.y.toFloat); buf.put(0.0f)
    buf.put(basisY.x.toFloat); buf.put(basisY.y.toFloat); buf.put(0.0f)
    buf.put(origin.x.toFloat); buf.put(origin.y.toFloat); buf.put(1.0f)
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }
}

object Frame2d {
  /** Create an identity coordinate frame at the world origin with unit basis vectors. */
  def apply() =
    new Frame2d(Vec2d.unitX, Vec2d.unitY, Pos2d.origin)

  /** Create a coordinate frame at the world origin with the given basis vectors. */
  def apply(basisX: Vec2d, basisY: Vec2d) =
    new Frame2d(basisX, basisY, Pos2d.origin)

  /** Create an arbitrary coordinate frame. */
  def apply(basisX: Vec2d, basisY: Vec2d, origin: Pos2d) =
    new Frame2d(basisX, basisY, origin)

  /** Create an arbitrary coordinate frame from a nested list (basis vectors followed by origin)
    * of the corresponding 3x3 matrix.
    */
  def apply(mx: List[List[Double]]) =
    mx match {
      case List(List(bxx, bxy, 0.0),
        List(byx, byy, 0.0),
        List(ox, oy, 1.0)) => new Frame2d(Vec2d(bxx, bxy),
        Vec2d(byx, byy),
        Pos2d(ox, oy))
      case _ => throw new IllegalArgumentException(
        "The given nested list of values did not correspond to a legal 3x3 matrix!")
    }

  /** Create an arbitrary coordinate frame from a nested array (basis vectors followed by origin)
    * of the corresponding 3x3 matrix.
    */
  def apply(mx: Array[Array[Double]]) =
    mx match {
      case Array(Array(bxx, bxy, 0.0),
        Array(byx, byy, 0.0),
        Array(ox, oy, 1.0)) => new Frame2d(Vec2d(bxx, bxy),
        Vec2d(byx, byy),
        Pos2d(ox, oy))
      case _ => throw new IllegalArgumentException(
        "The given nested array of values did not correspond to a legal 3x3 matrix!")
    }

  /** Create an arbitrary coordinate frame from a native array (basis vectors followed by origin)
    * of the corresponding 3x3 matrix.
    */
  def apply(mx: DoubleBuffer) = {
    if (mx.capacity != 9)
      throw new IllegalArgumentException(
        "The given native array did not contain (9) values!")
    mx.rewind
    (mx.get, mx.get, mx.get,
      mx.get, mx.get, mx.get,
      mx.get, mx.get, mx.get) match {
        case (bxx, bxy, 0.0,
          byx, byy, 0.0,
          ox, oy, 1.0) => new Frame2d(Vec2d(bxx, bxy),
          Vec2d(byx, byy),
          Pos2d(ox, oy))
        case _ => throw new IllegalArgumentException(
          "The given native array of values did not correspond to a legal 3x3 matrix!")
      }
  }

  /** Create an arbitrary coordinate frame from a native array (basis vectors followed by origin)
    * of the corresponding 3x3 matrix of floats.
    */
  def apply(mx: FloatBuffer) = {
    if (mx.capacity != 9)
      throw new IllegalArgumentException(
        "The given native array did not contain (9) values!")
    mx.rewind
    (mx.get, mx.get, mx.get,
      mx.get, mx.get, mx.get,
      mx.get, mx.get, mx.get) match {
        case (bxx, bxy, 0.0f,
          byx, byy, 0.0f,
          ox, oy, 1.0f) => new Frame2d(Vec2d(bxx.toDouble, bxy.toDouble),
          Vec2d(byx.toDouble, byy.toDouble),
          Pos2d(ox.toDouble, oy.toDouble))
        case _ => throw new IllegalArgumentException(
          "The given native array of values did not correspond to a legal 3x3 matrix!")
      }
  }

  /** Create a new uniform scaling coordinate frame.
    *
    * |  s  0.0 0.0 |
    * | 0.0  s  0.0 |
    * | 0.0 0.0 1.0 |
    */
  def scale(s: Double) =
    Frame2d(Vec2d.unitX * s, Vec2d.unitY * s, Pos2d.origin)

  /** Create a new non-uniform scaling coordinate frame.
    *
    * |  x  0.0 0.0 |
    * | 0.0  y  0.0 |
    * | 0.0 0.0 1.0 |
    */
  def scale(v: Vec2d) =
    Frame2d(Vec2d.unitX * v.x, Vec2d.unitY * v.y, Pos2d.origin)

  /** Create a new non-uniform scaling coordinate frame.
    *
    * |  x  0.0 0.0 |
    * | 0.0  y  0.0 |
    * | 0.0 0.0 1.0 |
    */
  def scale(x: Double, y: Double) =
    Frame2d(Vec2d.unitX * x, Vec2d.unitY * y, Pos2d.origin)

  /** Create a new translation coordinate frame.
    *
    * | 1.0 0.0  x  |
    * | 0.0 1.0  y  |
    * | 0.0 0.0 1.0 |
    */
  def translate(v: Vec2d) =
    Frame2d(Vec2d.unitX, Vec2d.unitY, v.toPos2d)

  /** Create a new translation coordinate frame.
    *
    * | 1.0 0.0  x  |
    * | 0.0 1.0  y  |
    * | 0.0 0.0 1.0 |
    */
  def translate(x: Double, y: Double) =
    Frame2d(Vec2d.unitX, Vec2d.unitY, Pos2d(x, y))

  /** Create a new rotation coordinate frame described by a counter-clockwise rotation of
    * the given number of radians.
    */
  def rotate(angle: Double) = {
    val s = sin(angle)
    val c = cos(angle)
    Frame2d(Vec2d(c, s), Vec2d(-s, c), Pos2d.origin)
  }
}

