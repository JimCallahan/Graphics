// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math2

import java.nio.{ CharBuffer, ShortBuffer, IntBuffer, LongBuffer, FloatBuffer, DoubleBuffer }

class Index3i private (val x: Int, val y: Int, val z: Int)
  extends Tuple3[Int, Index3i]
  with TupleOps[Int, Index3i, Index3i] {

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Index3i =>
        (that canEqual this) && (x == that.x) && (y == that.y) && (z == that.z)
      case _ => false
    }

  def canEqual(that: Any): Boolean = that.isInstanceOf[Index3i]

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 47 * (43 * (41 + x.##) + y.##) + z.##

  def apply(i: Int): Int =
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def update(i: Int, e: Int): Index3i =
    i match {
      case 0 => Index3i(e, y, z)
      case 1 => Index3i(x, e, z)
      case 2 => Index3i(x, y, e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateX(e: Int): Index3i = Index3i(e, y, z)
  def updateY(e: Int): Index3i = Index3i(x, e, z)
  def updateZ(e: Int): Index3i = Index3i(x, y, e)

  def dot(that: Index3i): Int = x * that.x + y * that.y + z + that.z

  def - : Index3i = Index3i(-x, -y, -z)

  def +(that: Index3i): Index3i = Index3i(x + that.x, y + that.y, z + that.z)
  def -(that: Index3i): Index3i = Index3i(x - that.x, y - that.y, z - that.z)
  def *(that: Index3i): Index3i = Index3i(x * that.x, y * that.y, z * that.z)
  def /(that: Index3i): Index3i = Index3i(x / that.x, y / that.y, z / that.z)

  def +(s: Int): Index3i = Index3i(x + s, y + s, z + s)
  def -(s: Int): Index3i = Index3i(x - s, y - s, z - s)
  def *(s: Int): Index3i = Index3i(x * s, y * s, z * s)
  def /(s: Int): Index3i = Index3i(x / s, y / s, z / s)

  def forall(p: (Int) => Boolean): Boolean = p(x) && p(y) && p(z)
  def forall(that: Index3i)(p: (Int, Int) => Boolean): Boolean = p(x, that.x) && p(y, that.y) && p(z, that.z)
  def equiv(that: Index3i, epsilon: Int): Boolean = forall(that)(Scalar.equiv(_, _, epsilon)) 
  def equiv(that: Index3i): Boolean = forall(that)(Scalar.equiv(_, _)) 

  def forany(p: (Int) => Boolean): Boolean = p(x) || p(y) || p(z)
  def forany(that: Index3i)(p: (Int, Int) => Boolean): Boolean = p(x, that.x) || p(y, that.y) || p(z, that.z)

  def foreach(p: (Int) => Unit): Unit = { p(x); p(y); p(z) }

  def map(p: (Int) => Int): Index3i = Index3i(p(x), p(y), p(z))

  def foldLeft[A](start: A)(f: (A, Int) => A): A = f(f(f(start, x), y), z)
  def /:[A](start: A)(f: (A, Int) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Int, A) => A): A = f(x, f(y, f(z, start)))
  def :\[A](start: A)(f: (Int, A) => A): A = foldRight(start)(f)

  def reduce(p: (Int, Int) => Int): Int = p(x, p(y, z))
  def min: Int = reduce(_ min _)
  def max: Int = reduce(_ max _)

  def compwise(that: Index3i)(p: (Int, Int) => Int): Index3i =
    Index3i(p(x, that.x), p(y, that.y), p(z, that.z))
  def min(that: Index3i): Index3i = compwise(that)(_ min _)
  def max(that: Index3i): Index3i = compwise(that)(_ max _)

  def compwise(a: Index3i, b: Index3i)(p: (Int, Int, Int) => Int): Index3i =
    Index3i(p(x, a.x, b.x), p(y, a.y, b.y), p(z, a.z, b.z))
  def clamp(lower: Index3i, upper: Index3i): Index3i = compwise(lower, upper)(Scalar.clamp)

  /** Convert to a String representation */
  override def toString() = "Index3i(%d, %d, %d)".format(x, y, z)

  def toList: List[Int] = List(x, y, z)
  def toArray: Array[Int] = Array(x, y, z)

  def toVec4d: Vec4d = Vec4d(x.toDouble, y.toDouble, z.toDouble, 0.0)
  def toVec3d: Vec3d = Vec3d(x.toDouble, y.toDouble, z.toDouble)
  def toVec2d: Vec2d = Vec2d(x.toDouble, y.toDouble)

  def toPos3d: Pos3d = Pos3d(x.toDouble, y.toDouble, z.toDouble)
  def toPos2d: Pos2d = Pos2d(x.toDouble, y.toDouble)

  def toIndex3i: Index3i = this
  def toIndex2i: Index2i = Index2i(x, y)

  def putNative(buf: CharBuffer) {
    buf.put(x.toChar); buf.put(y.toChar); buf.put(z.toChar)
  }
  def >>>(buf: CharBuffer) { putNative(buf) }

  def putNative(buf: ShortBuffer) {
    buf.put(x.toShort); buf.put(y.toShort); buf.put(z.toShort)
  }
  def >>>(buf: ShortBuffer) { putNative(buf) }

  def putNative(buf: IntBuffer) {
    buf.put(x); buf.put(y); buf.put(z)
  }
  def >>>(buf: IntBuffer) { putNative(buf) }

  def putNative(buf: LongBuffer) {
    buf.put(x.toLong); buf.put(y.toLong); buf.put(z.toLong)
  }
  def >>>(buf: LongBuffer) { putNative(buf) }

  def putNative(buf: FloatBuffer) {
    buf.put(x.toFloat); buf.put(y.toFloat); buf.put(z.toFloat)
  }
  def >>>(buf: FloatBuffer) { putNative(buf) }

  def putNative(buf: DoubleBuffer) {
    buf.put(x.toDouble); buf.put(y.toDouble); buf.put(z.toDouble)
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }
}

object Index3i {
  def apply(s: Int): Index3i = new Index3i(s, s, s)
  def apply(x: Int, y: Int, z: Int): Index3i = new Index3i(x, y, z)
}
