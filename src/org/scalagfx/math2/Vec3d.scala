package org.scalagfx.math2

import java.nio.{ CharBuffer, ShortBuffer, IntBuffer, LongBuffer, FloatBuffer, DoubleBuffer }

class Vec3d private (val x: Double, val y: Double, val z: Double)
  extends Tuple3[Double, Vec3d]
  with DoubleTupleOps[Vec3d]
  with VecOps[Double, Vec3d]
  with Vector {
  
  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Vec3d =>
        (that canEqual this) && (x == that.x) && (y == that.y) && (z == that.z)
      case _ => false
    }

  def canEqual(that: Any): Boolean = that.isInstanceOf[Vec3d]

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 47 * (43 * (41 + x.##) + y.##) + z.##

  def apply(i: Int): Double =
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def update(i: Int, e: Double): Vec3d =
    i match {
      case 0 => Vec3d(e, y, z)
      case 1 => Vec3d(x, e, z)
      case 2 => Vec3d(x, y, e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateX(e: Double): Vec3d = Vec3d(e, y, z)
  def updateY(e: Double): Vec3d = Vec3d(x, e, z)
  def updateZ(e: Double): Vec3d = Vec3d(x, y, e)

  def magSq: Double = dot(this)
  def mag: Double = scala.math.sqrt(magSq)
  def normalized: Vec3d = this / mag

  def dot(that: Vec3d): Double = x * that.x + y * that.y + z * that.z
  def cross(that: Vec3d): Vec3d =
    Vec3d(y * that.z - z * that.y, z * that.x - x * that.z, x * that.y - y * that.x)

  def - : Vec3d = Vec3d(-x, -y, -z)

  def +(that: Vec3d): Vec3d = Vec3d(x + that.x, y + that.y, z + that.z)
  def -(that: Vec3d): Vec3d = Vec3d(x - that.x, y - that.y, z - that.z)
  def *(that: Vec3d): Vec3d = Vec3d(x * that.x, y * that.y, z * that.z)
  def /(that: Vec3d): Vec3d = Vec3d(x / that.x, y / that.y, z / that.z)

  def +(s: Double): Vec3d = Vec3d(x + s, y + s, z + s)
  def -(s: Double): Vec3d = Vec3d(x - s, y - s, z - s)
  def *(s: Double): Vec3d = Vec3d(x * s, y * s, z * s)
  def /(s: Double): Vec3d = Vec3d(x / s, y / s, z / s)

  def forall(p: (Double) => Boolean): Boolean = p(x) && p(y) && p(z)
  def forall(that: Vec3d)(p: (Double, Double) => Boolean): Boolean = p(x, that.x) && p(y, that.y) && p(z, that.z)
  def equiv(that: Vec3d, epsilon: Double): Boolean = forall(that)(Scalar.equiv(_, _, epsilon)) 
  def equiv(that: Vec3d): Boolean = forall(that)(Scalar.equiv(_, _)) 
  
  def forany(p: (Double) => Boolean): Boolean = p(x) || p(y) || p(z)
  def forany(that: Vec3d)(p: (Double, Double) => Boolean): Boolean = p(x, that.x) || p(y, that.y) || p(z, that.z)

  def foreach(p: (Double) => Unit): Unit = { p(x); p(y); p(z) }

  def map(p: (Double) => Double): Vec3d = Vec3d(p(x), p(y), p(z))

  def foldLeft[A](start: A)(f: (A, Double) => A): A = f(f(f(start, x), y), z)
  def /:[A](start: A)(f: (A, Double) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Double, A) => A): A = f(x, f(y, f(z, start)))
  def :\[A](start: A)(f: (Double, A) => A): A = foldRight(start)(f)

  def reduce(p: (Double, Double) => Double): Double = p(x, p(y, z))
  def min: Double = reduce(_ min _)
  def max: Double = reduce(_ max _)

  def compwise(that: Vec3d)(p: (Double, Double) => Double): Vec3d =
    Vec3d(p(x, that.x), p(y, that.y), p(z, that.z))
  def min(that: Vec3d): Vec3d = compwise(that)(_ min _)
  def max(that: Vec3d): Vec3d = compwise(that)(_ max _)
  def lerp(that: Vec3d, t: Double): Vec3d = compwise(that)(Scalar.lerp(_, _, t))
  def smoothlerp(that: Vec3d, t: Double): Vec3d = compwise(that)(Scalar.smoothlerp(_, _, t))
  
  def compwise(a: Vec3d, b: Vec3d)(p: (Double, Double, Double) => Double): Vec3d =
    Vec3d(p(x, a.x, b.x), p(y, a.y, b.y), p(z, a.z, b.z))
  def clamp(lower: Vec3d, upper: Vec3d): Vec3d = compwise(lower, upper)(Scalar.clamp)

  /** Convert to a String representation */
  override def toString() = "Vec3d(%.2f, %.2f, %.2f)".format(x, y, z)

  def toList: List[Double] = List(x, y, z)
  def toArray: Array[Double] = Array(x, y, z)

  def toVec4d: Vec4d = Vec4d(x, y, z, 0.0)
  def toVec3d: Vec3d = this
  def toVec2d: Vec2d = Vec2d(x, y)

  def toPos3d: Pos3d = Pos3d(x, y, z)
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

object Vec3d {
  def apply(s: Double): Vec3d = new Vec3d(s, s, s)
  def apply(x: Double, y: Double, z: Double): Vec3d = new Vec3d(x, y, z)
  def random: Vec3d = new Vec3d(scala.math.random, scala.math.random, scala.math.random)
  def randomUnit: Vec3d = {
    val v = random - Vec3d(0.5)
    val ms = v.magSq
    if ((ms < 0.25) && (ms > 0.001)) v / scala.math.sqrt(ms)
    else randomUnit
  }
  val unitX: Vec3d = Vec3d(1.0, 0.0, 0.0)
  val unitY: Vec3d = Vec3d(0.0, 1.0, 0.0) 
  val unitZ: Vec3d = Vec3d(0.0, 0.0, 1.0)
}
