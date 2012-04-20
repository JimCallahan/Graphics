// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math2

import java.nio.{ CharBuffer, ShortBuffer, IntBuffer, LongBuffer, FloatBuffer, DoubleBuffer }

class Vec4d private (val x: Double, val y: Double, val z: Double, val w: Double)
  extends Tuple3[Double, Vec4d]
  with DoubleTupleOps[Vec4d]
  with VecOps[Double, Vec4d]
  with Vector {
  
  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Vec4d =>
        (that canEqual this) && (x == that.x) && (y == that.y) && (z == that.z) && (w == that.w)
      case _ => false
    }

  def canEqual(that: Any): Boolean = that.isInstanceOf[Vec4d]

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 49 * (47 * (43 * (41 + x.##) + y.##) + z.##) + w.##

  def apply(i: Int): Double =
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case 3 => w
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def update(i: Int, e: Double): Vec4d =
    i match {
      case 0 => Vec4d(e, y, z, w)
      case 1 => Vec4d(x, e, z, w)
      case 2 => Vec4d(x, y, e, w)
      case 3 => Vec4d(x, y, z, e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateX(e: Double): Vec4d = Vec4d(e, y, z, w)
  def updateY(e: Double): Vec4d = Vec4d(x, e, z, w)
  def updateZ(e: Double): Vec4d = Vec4d(x, y, e, w)
  def updateW(e: Double): Vec4d = Vec4d(x, y, z, e)

  def magSq: Double = dot(this)
  def mag: Double = scala.math.sqrt(magSq)
  def normalized: Vec4d = this / mag

  def dot(that: Vec4d): Double = x * that.x + y * that.y + z * that.z + w * that.w

  def - : Vec4d = Vec4d(-x, -y, -z, -w)

  def +(that: Vec4d): Vec4d = Vec4d(x + that.x, y + that.y, z + that.z, w + that.w)
  def -(that: Vec4d): Vec4d = Vec4d(x - that.x, y - that.y, z - that.z, w - that.w)
  def *(that: Vec4d): Vec4d = Vec4d(x * that.x, y * that.y, z * that.z, w * that.w)
  def /(that: Vec4d): Vec4d = Vec4d(x / that.x, y / that.y, z / that.z, w / that.w)

  def +(s: Double): Vec4d = Vec4d(x + s, y + s, z + s, w + s)
  def -(s: Double): Vec4d = Vec4d(x - s, y - s, z - s, w - s)
  def *(s: Double): Vec4d = Vec4d(x * s, y * s, z * s, w * s)
  def /(s: Double): Vec4d = Vec4d(x / s, y / s, z / s, w / s)

  def forall(p: (Double) => Boolean): Boolean = p(x) && p(y) && p(z) && p(w)
  def forall(that: Vec4d)(p: (Double, Double) => Boolean): Boolean = p(x, that.x) && p(y, that.y) && p(z, that.z) && p(w, that.w)
  def equiv(that: Vec4d, epsilon: Double): Boolean = forall(that)(Scalar.equiv(_, _, epsilon)) 
  def equiv(that: Vec4d): Boolean = forall(that)(Scalar.equiv(_, _)) 
  
  def forany(p: (Double) => Boolean): Boolean = p(x) || p(y) || p(z) || p(w)
  def forany(that: Vec4d)(p: (Double, Double) => Boolean): Boolean = p(x, that.x) || p(y, that.y) || p(z, that.z) || p(w, that.w)

  def foreach(p: (Double) => Unit): Unit = { p(x); p(y); p(z); p(w) }

  def map(p: (Double) => Double): Vec4d = Vec4d(p(x), p(y), p(z), p(w))

  def foldLeft[A](start: A)(f: (A, Double) => A): A = f(f(f(f(start, x), y), z), w)
  def /:[A](start: A)(f: (A, Double) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Double, A) => A): A = f(x, f(y, f(z, f(w, start))))
  def :\[A](start: A)(f: (Double, A) => A): A = foldRight(start)(f)

  def reduce(p: (Double, Double) => Double): Double = p(x, p(y, p(z, w)))
  def min: Double = reduce(_ min _)
  def max: Double = reduce(_ max _)

  def compwise(that: Vec4d)(p: (Double, Double) => Double): Vec4d =
    Vec4d(p(x, that.x), p(y, that.y), p(z, that.z), p(w, that.w))
  def min(that: Vec4d): Vec4d = compwise(that)(_ min _)
  def max(that: Vec4d): Vec4d = compwise(that)(_ max _)
  def lerp(that: Vec4d, t: Double): Vec4d = compwise(that)(Scalar.lerp(_, _, t))
  def smoothlerp(that: Vec4d, t: Double): Vec4d = compwise(that)(Scalar.smoothlerp(_, _, t))
  
  def compwise(a: Vec4d, b: Vec4d)(p: (Double, Double, Double) => Double): Vec4d =
    Vec4d(p(x, a.x, b.x), p(y, a.y, b.y), p(z, a.z, b.z), p(w, a.w, b.w))
  def clamp(lower: Vec4d, upper: Vec4d): Vec4d = compwise(lower, upper)(Scalar.clamp)

  /** Convert to a String representation */
  override def toString() = "Vec4d(%.2f, %.2f, %.2f, %.2f)".format(x, y, z, w)

  def toList: List[Double] = List(x, y, z, w)
  def toArray: Array[Double] = Array(x, y, z, w)

  def toVec4d: Vec4d = this
  def toVec3d: Vec3d = Vec3d(x, y, z)
  def toVec2d: Vec2d = Vec2d(x, y)

  def toPos3d: Pos3d = Pos3d(x, y, z)
  def toPos2d: Pos2d = Pos2d(x, y)

  def toIndex3i: Index3i = Index3i(x.toInt, y.toInt, z.toInt)
  def toIndex2i: Index2i = Index2i(x.toInt, y.toInt)

  def putNative(buf: CharBuffer) {
    buf.put(x.toChar); buf.put(y.toChar); buf.put(z.toChar); buf.put(w.toChar)
  }
  def >>>(buf: CharBuffer) { putNative(buf) }

  def putNative(buf: ShortBuffer) {
    buf.put(x.toShort); buf.put(y.toShort); buf.put(z.toShort); buf.put(w.toShort)
  }
  def >>>(buf: ShortBuffer) { putNative(buf) }

  def putNative(buf: IntBuffer) {
    buf.put(x.toInt); buf.put(y.toInt); buf.put(z.toInt); buf.put(w.toInt)
  }
  def >>>(buf: IntBuffer) { putNative(buf) }

  def putNative(buf: LongBuffer) {
    buf.put(x.toLong); buf.put(y.toLong); buf.put(z.toLong); buf.put(w.toLong)
  }
  def >>>(buf: LongBuffer) { putNative(buf) }

  def putNative(buf: FloatBuffer) {
    buf.put(x.toFloat); buf.put(y.toFloat); buf.put(z.toFloat); buf.put(w.toFloat)
  }
  def >>>(buf: FloatBuffer) { putNative(buf) }

  def putNative(buf: DoubleBuffer) {
    buf.put(x); buf.put(y); buf.put(z); buf.put(w)
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }
}

object Vec4d {
  def apply(s: Double): Vec4d = new Vec4d(s, s, s, s)
  def apply(x: Double, y: Double, z: Double, w: Double): Vec4d = new Vec4d(x, y, z, w)
  def random: Vec4d = new Vec4d(scala.math.random, scala.math.random, scala.math.random, scala.math.random)
  def randomUnit: Vec4d = {
    val v = random - Vec4d(0.5)
    val ms = v.magSq
    if ((ms < 0.25) && (ms > 0.001)) v / scala.math.sqrt(ms)
    else randomUnit
  }
  val unitX: Vec4d = Vec4d(1.0, 0.0, 0.0, 0.0)
  val unitY: Vec4d = Vec4d(0.0, 1.0, 0.0, 0.0) 
  val unitZ: Vec4d = Vec4d(0.0, 0.0, 1.0, 0.0) 
  val unitW: Vec4d = Vec4d(0.0, 0.0, 0.0, 1.0) 
}
