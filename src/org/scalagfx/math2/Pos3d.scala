package org.scalagfx.math2

import java.nio.{ CharBuffer, ShortBuffer, IntBuffer, LongBuffer, FloatBuffer, DoubleBuffer }

class Pos3d private (val x: Double, val y: Double, val z: Double)
  extends Tuple3[Double, Pos3d]
  with DoubleTupleOps[Pos3d]
  with PosOps[Double, Pos3d, Vec3d]
  with Position {
  
  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Pos3d =>
        (that canEqual this) && (x == that.x) && (y == that.y) && (z == that.z)
      case _ => false
    }

  def canEqual(that: Any): Boolean = that.isInstanceOf[Pos3d]

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 47 * (43 * (41 + x.##) + y.##) + z.##
    
  def apply(i: Int): Double =
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def update(i: Int, e: Double): Pos3d =
    i match {
      case 0 => Pos3d(e, y, z)
      case 1 => Pos3d(x, e, z)
      case 2 => Pos3d(x, y, e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateX(e: Double): Pos3d = Pos3d(e, y, z)
  def updateY(e: Double): Pos3d = Pos3d(x, e, z)
  def updateZ(e: Double): Pos3d = Pos3d(x, y, e)

  def - : Pos3d = Pos3d(-x, -y, -z)
  def -(that: Pos3d): Vec3d = Vec3d(x - that.x, y - that.y, z - that.z)

  def +(that: Vec3d): Pos3d = Pos3d(x + that.x, y + that.y, z + that.z)
  def -(that: Vec3d): Pos3d = Pos3d(x - that.x, y - that.y, z - that.z)
  def *(that: Vec3d): Pos3d = Pos3d(x * that.x, y * that.y, z * that.z)
  def /(that: Vec3d): Pos3d = Pos3d(x / that.x, y / that.y, z / that.z)

  def +(s: Double): Pos3d = Pos3d(x + s, y + s, z + s)
  def -(s: Double): Pos3d = Pos3d(x - s, y - s, z - s)
  def *(s: Double): Pos3d = Pos3d(x * s, y * s, z * s)
  def /(s: Double): Pos3d = Pos3d(x / s, y / s, z / s)

  def forall(p: (Double) => Boolean): Boolean = p(x) && p(y) && p(z)
  def forall(that: Pos3d)(p: (Double, Double) => Boolean): Boolean = p(x, that.x) && p(y, that.y) && p(z, that.z)
  def equiv(that: Pos3d, epsilon: Double): Boolean = forall(that)(Scalar.equiv(_, _, epsilon)) 
  def equiv(that: Pos3d): Boolean = forall(that)(Scalar.equiv(_, _)) 

  def forany(p: (Double) => Boolean): Boolean = p(x) || p(y) || p(z)
  def forany(that: Pos3d)(p: (Double, Double) => Boolean): Boolean = p(x, that.x) || p(y, that.y) || p(z, that.z)

  def foreach(p: (Double) => Unit): Unit = { p(x); p(y); p(z) }

  def map(p: (Double) => Double): Pos3d = Pos3d(p(x), p(y), p(z))

  def foldLeft[A](start: A)(f: (A, Double) => A): A = f(f(f(start, x), y), z)
  def /:[A](start: A)(f: (A, Double) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Double, A) => A): A = f(x, f(y, f(z, start)))
  def :\[A](start: A)(f: (Double, A) => A): A = foldRight(start)(f)

  def reduce(p: (Double, Double) => Double): Double = p(x, p(y, z))
  def min: Double = reduce(_ min _)
  def max: Double = reduce(_ max _)

  def compwise(that: Pos3d)(p: (Double, Double) => Double): Pos3d =
    Pos3d(p(x, that.x), p(y, that.y), p(z, that.z))
  def min(that: Pos3d): Pos3d = compwise(that)(_ min _)
  def max(that: Pos3d): Pos3d = compwise(that)(_ max _)
  def lerp(that: Pos3d, t: Double): Pos3d = compwise(that)(Scalar.lerp(_, _, t))
  def smoothlerp(that: Pos3d, t: Double): Pos3d = compwise(that)(Scalar.smoothlerp(_, _, t))
  
  def compwise(a: Pos3d, b: Pos3d)(p: (Double, Double, Double) => Double): Pos3d =
    Pos3d(p(x, a.x, b.x), p(y, a.y, b.y), p(z, a.z, b.z))
  def clamp(lower: Pos3d, upper: Pos3d): Pos3d = compwise(lower, upper)(Scalar.clamp)

  /** Convert to a String representation */
  override def toString() = "Pos3d(%.2f, %.2f, %.2f)".format(x, y, z)

  def toList: List[Double] = List(x, y, z)
  def toArray: Array[Double] = Array(x, y, z)

  def toVec4d: Vec4d = Vec4d(x, y, z, 0.0)
  def toVec3d: Vec3d = Vec3d(x, y, z)
  def toVec2d: Vec2d = Vec2d(x, y)

  def toPos3d: Pos3d = this
  def toPos2d: Pos2d = Pos2d(x, y)

  def toIndex3i: Index3i = Index3i(x.toInt, y.toInt, z.toInt)
  def toIndex2i: Index2i = Index2i(x.toInt, y.toInt)

  def putNative(buf: CharBuffer) {
    buf.put(x.toChar); buf.put(y.toChar); buf.put(z.toChar)
  }
  def >>>(buf: CharBuffer) { putNative(buf) }

  def putNative(buf: ShortBuffer) {
    buf.put(x.toShort); buf.put(y.toShort); buf.put(z.toShort)
  }
  def >>>(buf: ShortBuffer) { putNative(buf) }

  def putNative(buf: IntBuffer) {
    buf.put(x.toInt); buf.put(y.toInt); buf.put(z.toInt)
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
    buf.put(x); buf.put(y); buf.put(z)
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }
}

object Pos3d {
  def apply(s: Double): Pos3d = new Pos3d(s, s, s)
  def apply(x: Double, y: Double, z: Double): Pos3d = new Pos3d(x, y, z)
  val origin = Pos3d(0.0)
}
