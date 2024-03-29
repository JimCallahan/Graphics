// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math

import java.nio._

//--------------------------------------------------------------------------------------------------
//   I N D E X   3 I  
//--------------------------------------------------------------------------------------------------

/** Companion object for Index3i. */
object Index3i 
{
  //------------------------------------------------------------------------------------------------
  //   C R E A T I O N                                                                     
  //------------------------------------------------------------------------------------------------

  /** Create a new index from components. */
  def apply(x: Int, y: Int, z: Int) = 
    new Index3i(x, y, z)

  /** Create a new index in which all components are the same scalar value. */ 
  def apply(s: Int) = 
    new Index3i(s, s, s) 
 

  /** A zero length index. */
  val zero: Index3i = 
    Index3i(0)

  /** A index with all components equal to (1). */
  val one: Index3i = 
    Index3i(1)


  /** A unit length index along the X-axis. */ 
  val unitX: Index3i = 
    Index3i(1, 0, 0)

  /** A unit length index along the Y-axis. */ 
  val unitY: Index3i = 
    Index3i(0, 1, 0) 

  /** A unit length index along the Z-axis. */ 
  val unitZ: Index3i = 
    Index3i(0, 0, 1) 
  

  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** The component-wise minimum of two indices. */
  def min(a: Index3i, b: Index3i): Index3i = 
    compwise(a, b, scala.math.min(_, _)) 
  
  /** The component-wise maximum of two indices. */
  def max(a: Index3i, b: Index3i): Index3i = 
    compwise(a, b, scala.math.max(_, _)) 
  			
  
  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                        
  //------------------------------------------------------------------------------------------------

  /** Create a index who's components are generated by applying the given binary operator
    * to each of the corresponding components of the given two indices. */ 
  def compwise(a: Index3i, b: Index3i, f: (Int, Int) => Int): Index3i = 
    Index3i(f(a.x, b.x), f(a.y, b.y), f(a.z, b.z))
}

/** An immutable 3-dimensional index of Int element type used to represent cell indices of a 
  * regular subdivision of space (voxels). */ 
class Index3i(val x: Int, val y: Int, val z: Int) 
{
  //------------------------------------------------------------------------------------------------
  //   C O M P O N E N T   O P S                                                            
  //------------------------------------------------------------------------------------------------

  /** A copy of this index in which the X component has been replaced with the given
    * value. */ 
  def newX(v: Int): Index3i = 
    Index3i(v, y, z)
  
  /** A copy of this index in which the Y component has been replaced with the given
    * value. */ 
  def newY(v: Int): Index3i = 
    Index3i(x, v, z)
  
  /** A copy of this index in which the Z component has been replaced with the given
    * value. */ 
  def newZ(v: Int): Index3i = 
    Index3i(x, y, v)
  
  /** A copy of this index in which the component with the given index as been replaced. */
  def newComp(i: Int, v: Int) = 
    i match {
      case 0 => Index3i(v, y, z)
      case 1 => Index3i(x, v, z)
      case 2 => Index3i(x, y, v)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }
  

  //------------------------------------------------------------------------------------------------
  //   U N A R Y   O P S                                                                    
  //------------------------------------------------------------------------------------------------

  /** The number of dimensions. */
  val dimens = 3

  /** A index of identical magnitude but opposite direction. */
  def negated: Index3i = Index3i(-x, -y, -z)

  /** Get the minimum valued component. */
  def minComp: Int = 
    if(x < y) (if(x < z) x else z)
    else if(y < z) y else z

  /** Get the maximum valued component. */
  def maxComp: Int = 
    if(x > y) (if(x > z) x else z)
    else if(y > z) y else z
  
  /** A index who's components are the absolute value of the corresponding components of
    * this index. */ 
  def abs: Index3i = 
    map(scala.math.abs _) 
  			

  //------------------------------------------------------------------------------------------------
  //   O P E R A T O R S                                                                    
  //------------------------------------------------------------------------------------------------

  /** The addition of a scalar value to all components of this index. */ 
  def + (scalar: Int): Index3i = Index3i(x+scalar, y+scalar, z+scalar) 

  /** The component-wise addition of another index with this index. */ 
  def + (that: Index3i): Index3i = Index3i(x+that.x, y+that.y, z+that.z)
  

  /** The subtraction of a scalar value to all components of this index. */ 
  def - (scalar: Int): Index3i = Index3i(x-scalar, y-scalar, z-scalar) 
  
  /** The component-wise subtraction of another index from this index. */ 
  def - (that: Index3i): Index3i = Index3i(x-that.x, y-that.y, z-that.z)


  /** The product of a scalar value with all components of this index. */ 
  def * (scalar: Int): Index3i = Index3i(x*scalar, y*scalar, z*scalar) 

  /** The component-wise multiplication of another index with this index. */ 
  def * (that: Index3i): Index3i = Index3i(x*that.x, y*that.y, z*that.z)


  /** The dot-product of this and another index. */
  def dot(that: Index3i): Int = 
    x*that.x + y*that.y + z*that.z

  /** The cross-product of this and another index. */
  def cross(that: Index3i): Index3i = 
    Index3i(y * that.z - z * that.y,					    
            z * that.x - x * that.z,				    
            x * that.y - y * that.x)		
  

  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Compares this index to the specified value for equality. */
  override def equals(that: Any): Boolean = 
    that match {
      case that: Index3i => 
        (that canEqual this) && (x == that.x) && (y == that.y) && (z == that.z)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean = 
    that.isInstanceOf[Index3i]

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 
    47 * (43 * (41 + x.##) + y.##) + z.##

  //------------------------------------------------------------------------------------------------

  /** Are any of the components of this index less-than the corresponding component of the
    * given index. */
  def anyLt(that: Index3i): Boolean = 
    forany(that)(_ < _) 

  /** Are any of the components of this index less-than or equal the corresponding
    * component of the given index. */
  def anyLte(that: Index3i): Boolean = 
    forany(that)(_ <= _) 

  /** Are any of the components of this index greater-than the corresponding component of
    * the given index. */
  def anyGt(that: Index3i): Boolean = 
    forany(that)(_ > _) 

  /** Are any of the components of this index greater-than or equal the corresponding
    * component of the given index. */
  def anyGte(that: Index3i): Boolean = 
    forany(that)(_ >= _) 

  //------------------------------------------------------------------------------------------------

  /** Are all of the components of this index less-than the corresponding component of the
    * given index. */
  def allLt(that: Index3i): Boolean = 
    forall(that)(_ < _) 

  /** Are all of the components of this index less-than or equal the corresponding component
    * of the given index. */
  def allLte(that: Index3i): Boolean = 
    forall(that)(_ <= _) 

  /** Are all of the components of this index greater-than the corresponding
    * component of the given index. */
  def allGt(that: Index3i): Boolean = 
    forall(that)(_ > _) 

  /** Are all of the components of this index greater-than or equal the corresponding
    * component of the given index. */
  def allGte(that: Index3i): Boolean = 
    forall(that)(_ >= _) 


  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                       
  //------------------------------------------------------------------------------------------------

  /** The index component at the given index. */
  def apply(i: Int): Int = 
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  /** Tests whether the given predicate holds true for all components of this index. */ 
  def forall(p: (Int) => Boolean): Boolean = 
    p(x) && p(y) && p(z)

  /** Tests whether the given predicate holds true for all of the corresponding components
    * of this and the given index. */ 
  def forall(that: Index3i)(p: (Int, Int) => Boolean): Boolean = 
    p(x, that.x) && p(y, that.y) && p(z, that.z)


  /** Tests whether the given predicate holds true for any component of this index. */ 
  def forany(p: (Int) => Boolean): Boolean = 
    p(x) || p(y) || p(z)

  /** Tests whether the given predicate holds true for any of the corresponding components
    * of this and the given index. */ 
  def forany(that: Index3i)(p: (Int, Int) => Boolean): Boolean = 
    p(x, that.x) || p(y, that.y) || p(z, that.z)

  /** Applies a function to all components of this index.
   *
   * @param f  The function that is applied for its side-effect to every component.  */
  def foreach(f: (Int) => Unit): Unit = { 
    f(x); f(y); f(z)
  }

  /** Builds a new index by applying a function to each component of this index. */
  def map(f: (Int) => Int): Index3i = 
    Index3i(f(x), f(y), f(z))

  /** Applies a binary operator to a start value and all components of this index, going
    * left to right. */
  def foldLeft[A](start: A)(f: (A, Int) => A): A = 
    f(f(f(start, x), y), z)

  /** Applies a binary operator to a start value and all components of this index, going
    * left to right. */
  def /: [A](start: A)(f: (A, Int) => A): A = 
    foldLeft(start)(f)

  /** Applies a binary operator to a start value and all components of this index,
    * going right to left. */
  def foldRight[A](start: A)(f: (Int, A) => A): A = 
    f(x, f(y, f(z, start)))

  /** Applies a binary operator to a start value and all components of this index,
    * going right to left. */
  def :\ [A](start: A)(f: (Int, A) => A): A = 
    foldRight(start)(f)

  /** Reduces the components of this index using the specified associative binary operator. */
  def reduce(f: (Int, Int) => Int): Int = 
    f(f(x, y), z)


  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a list of components. */ 
  def toList: List[Int] = List(x, y, z) 

  /** Convert to an array of components. */ 
  def toArray: Array[Int] = Array(x, y, z) 

  /** Convert to a 4-dimensional homogeneous vector. */ 
  def toVector4d: Vector4d = Vector4d(x.toDouble, y.toDouble, z.toDouble, 1.0) 

  /** Convert to a 3-dimensional vector. */ 
  def toVector3d: Vector3d = Vector3d(x.toDouble, y.toDouble, z.toDouble) 

  /** Convert to a 3-dimensional vector. */ 
  def toVector2d: Vector2d = Vector2d(x.toDouble, y.toDouble) 

  /** Convert to a vector from the origin to a this position. */ 
  def toVec3d: Vec3d = Vec3d(x.toDouble, y.toDouble, z.toDouble) 

  /** Convert to a vector from the origin to this position. */ 
  def toVec2d: Vec2d = Vec2d(x.toDouble, y.toDouble) 

  /** Convert to a 3-dimensional position. */ 
  def toPos3d: Pos3d = Pos3d(x.toDouble, y.toDouble, z.toDouble) 

  /** Convert to a 2-dimension.toDoubleal position. */ 
  def toPos2d: Pos2d = Pos2d(x.toDouble, y.toDouble) 

  /** Convert to a 3-dimensional index. */ 
  def toIndex3i: Index3i = this

  /** Convert to a 2-dimensional index. */ 
  def toIndex2i: Index2i = Index2i(x.toInt, y.toInt)

  /** Convert to a native array. */
  def toNative: IntBuffer = {
    val buf = ByteBuffer.allocateDirect(3 << 2).order(ByteOrder.nativeOrder).asIntBuffer
    putNative(buf)
    buf.rewind
    buf
  }

  /** Add the component values from this vector starting at the current buffer position to the
    * given native array. */ 
  def putNative(buf: IntBuffer) {
    buf.put(x); buf.put(y); buf.put(z)
  }

  /** Convert to a native array of shorts. */
  def toNativeShorts: ShortBuffer = {
    val buf = ByteBuffer.allocateDirect(3 << 1).order(ByteOrder.nativeOrder).asShortBuffer
    putNativeShorts(buf)
    buf.rewind
    buf
  }

  /** Add the component values from this vector starting at the current buffer position to the
    * given native array of shorts. */ 
  def putNativeShorts(buf: ShortBuffer) {
    buf.put(x.toShort); buf.put(y.toShort); buf.put(z.toShort)
  }

  /** Convert to a string representation. */
  override def toString() = 
    "Index3i(%d, %d, %d)".format(x, y, z)
}

