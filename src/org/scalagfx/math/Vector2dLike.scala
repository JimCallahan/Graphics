// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math

import java.nio._

//--------------------------------------------------------------------------------------------------
//   V E C T O R   2 D   L I K E                                                              
//--------------------------------------------------------------------------------------------------

/** A common set of methods for immutable 2-dimensional positions and vectors of Double
  * element type used in computational geometry applications. */
trait Vector2dLike 
{
  type Self

  //------------------------------------------------------------------------------------------------
  //   C O M P O N E N T S                                                                  
  //------------------------------------------------------------------------------------------------

  /** The X-component of the vector. */ 
  val x: Double

  /** The Y-component of the vector. */ 
  val y: Double


  //------------------------------------------------------------------------------------------------
  //   C O M P O N E N T   O P S                                                            
  //------------------------------------------------------------------------------------------------

  /** A copy of this vector in which the X-component has been replaced with the given
    * value. */ 
  def newX(v: Double): Self
  
  /** A copy of this vector in which the Y-component has been replaced with the given
    * value. */ 
  def newY(v: Double): Self
  
  /** A copy of this vector in which the component with the given index as been replaced. */
  def newComp(i: Int, v: Double): Self

  /** Decompose into components. */
  def decompose = (x, y)


  //------------------------------------------------------------------------------------------------
  //   U N A R Y   O P S                                                                    
  //------------------------------------------------------------------------------------------------

  /** The number of dimensions. */
  val dimens = 2

  /** Get the minimum valued component. */
  def minComp: Double = 
    if(x < y) x else y

  /** Get the maximum valued component. */
  def maxComp: Double = 
    if(x > y) x else y
  
  /** A vector who's components are the absolute value of the corrensponding components of
    * this vector. */ 
  def abs: Self = 
    map(scala.math.abs _) 
  	

  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether the given vector in within a given epsilon of
    * this vector. */ 
  def equiv(that: Self, epsilon: Double): Boolean = 
    forall(that)(Scalar.equiv(_, _, epsilon))
  
  /** The component-wise comparison of whether the given vector is within a type specific
    * epsilon of this vector. */ 
  def equiv(that: Self): Boolean = 
    forall(that)(Scalar.equiv(_, _))
  
  //------------------------------------------------------------------------------------------------

  /** Are any of the components of this vector less-than the corresponding component of the
    * given vector. */
  def anyLt(that: Self): Boolean = 
    forany(that)(_ < _) 

  /** Are any of the components of this vector less-than or equal the corresponding
    * component of the given vector. */
  def anyLte(that: Self): Boolean = 
    forany(that)(_ <= _) 

  /** Are any of the components of this vector greater-than the corresponding component of
    * the given vector. */
  def anyGt(that: Self): Boolean = 
    forany(that)(_ > _) 

  /** Are any of the components of this vector greater-than or equal the corresponding
    * component of the given vector. */
  def anyGte(that: Self): Boolean = 
    forany(that)(_ >= _) 

  //------------------------------------------------------------------------------------------------

  /** Are all of the components of this vector less-than the corresponding component of the
    * given vector. */
  def allLt(that: Self): Boolean = 
    forall(that)(_ < _) 

  /** Are all of the components of this vector less-than or equal the corresponding component
    * of the given vector. */
  def allLte(that: Self): Boolean = 
    forall(that)(_ <= _) 

  /** Are all of the components of this vector greater-than the corresponding
    * component of the given vector. */
  def allGt(that: Self): Boolean = 
    forall(that)(_ > _) 

  /** Are all of the components of this vector greater-than or equal the corresponding
    * component of the given vector. */
  def allGte(that: Self): Boolean = 
    forall(that)(_ >= _) 


  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                       
  //------------------------------------------------------------------------------------------------

  /** The vector component at the given index. */
  def apply(i: Int): Double = 
    i match {
      case 0 => x
      case 1 => y
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  /** Tests whether the given predicate holds true for all components of this vector. */ 
  def forall(p: (Double) => Boolean): Boolean = 
    p(x) && p(y)

  /** Tests whether the given predicate holds true for all of the corresponding components
    * of this and the given vector. */ 
  def forall(that: Self)(p: (Double, Double) => Boolean): Boolean


  /** Tests whether the given predicate holds true for any component of this vector. */ 
  def forany(p: (Double) => Boolean): Boolean = 
    p(x) || p(y)

  /** Tests whether the given predicate holds true for any of the corresponding components
    * of this and the given vector. */ 
  def forany(that: Self)(p: (Double, Double) => Boolean): Boolean

  /** Applies a function to all components of this vector.
   *
   * @param f  The function that is applied for its side-effect to every component.  */
  def foreach(f: (Double) => Unit): Unit = { 
    f(x); f(y)
  }

  /** Builds a new vector by applying a function to each component of this vector. */
  def map(f: (Double) => Double): Self

  /** Applies a binary operator to a start value and all components of this vector, going
    * left to right. */
  def foldLeft[A](start: A)(f: (A, Double) => A): A = 
    f(f(start, x), y)

  /** Applies a binary operator to a start value and all components of this vector, going
    * left to right. */
  def /: [A](start: A)(f: (A, Double) => A): A = foldLeft(start)(f)

  /** Applies a binary operator to a start value and all components of this vector,
    * going right to left. */
  def foldRight[A](start: A)(f: (Double, A) => A): A = 
    f(x, f(y, start))

  /** Applies a binary operator to a start value and all components of this vector,
    * going right to left. */
  def :\ [A](start: A)(f: (Double, A) => A): A = foldRight(start)(f)

  /** Reduces the components of this vector using the specified associative binary operator. */
  def reduce(f: (Double, Double) => Double): Double = 
    f(x, y)


  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a list of components. */ 
  def toList: List[Double] = List(x, y) 

  /** Convert to an array of components. */ 
  def toArray: Array[Double] = Array(x, y) 

  /** Convert to a 4-dimensional homogeneous vector. */ 
  def toVector4d: Vector4d = Vector4d(x, y, 0.0, 1.0) 

  /** Convert to a 3-dimensional vector. */ 
  def toVector3d: Vector3d = Vector3d(x, y, 0.0) 

  /** Convert to a 2-dimensional vector. */ 
  def toVector2d: Vector2d = Vector2d(x, y) 

  /** Convert to a 3-dimensional vector. */ 
  def toVec3d: Vec3d = Vec3d(x, y, 0.0) 

  /** Convert to a 2-dimensional vector. */ 
  def toVec2d: Vec2d = Vec2d(x, y) 

  /** Convert to a 3-dimensional position. */ 
  def toPos3d: Pos3d = Pos3d(x, y, 0.0) 

  /** Convert to a w-dimensional position. */ 
  def toPos2d: Pos2d = Pos2d(x, y) 

  /** Convert to a 3-dimensional index (ignoring the fractional part). */ 
  def toIndex3i: Index3i = Index3i(x.toInt, y.toInt, 0)

  /** Convert to a 2-dimensional index (ignoring the fractional part). */ 
  def toIndex2i: Index2i = Index2i(x.toInt, y.toInt) 
  
  /** Convert to a native array. */
  def toNative: DoubleBuffer = {
    val buf = ByteBuffer.allocateDirect(2 << 3).order(ByteOrder.nativeOrder).asDoubleBuffer
    putNative(buf)
    buf.rewind
    buf
  }

  /** Add the component values from this vector starting at the current position to given native
    * array. */ 
  def putNative(buf: DoubleBuffer) {
    buf.put(x); buf.put(y)
  }

  /** Add the component values from this vector starting at the current position to given native
    * array. */ 
  def -> (buf: DoubleBuffer) {
    putNative(buf)
  }

  /** Convert to a native array of floats. */
  def toNativeFloats: FloatBuffer = {
    val buf = ByteBuffer.allocateDirect(2 << 2).order(ByteOrder.nativeOrder).asFloatBuffer
    putNativeFloats(buf)
    buf.rewind
    buf
  }

  /** Add the component values from this vector starting at the current position to given native
    * array of floats. */ 
  def putNativeFloats(buf: FloatBuffer) {
    buf.put(x.toFloat); buf.put(y.toFloat)
  }

  /** Add the component values from this vector starting at the current position to given native
    * array of floats. */ 
  def -> (buf: FloatBuffer) {
    putNativeFloats(buf)
  }
}

