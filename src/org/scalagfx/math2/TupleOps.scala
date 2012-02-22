package org.scalagfx.math2

import java.nio.{ CharBuffer, ShortBuffer, IntBuffer, LongBuffer, FloatBuffer, DoubleBuffer }

trait TupleOps[@specialized(Double, Int) Elem, Repr <: Tuple, Alt <: Tuple] {  
  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean  
  
  /** Whether another vector is within a given epsilon of this vector. */ 
  def equiv(that: Repr, epsilon: Elem): Boolean

  /** Whether another vector is within a the standard epsilon of this vector. */ 
  def equiv(that: Repr): Boolean
    
  /** Negate all components. */
  def - : Repr

  /** The component-wise addition of this vector with another vector. */
  def +(that: Alt): Repr

  /** The component-wise subtraction of another vector from this vector. */
  def -(that: Alt): Repr

  /** The component-wise multiplication of this vector with another vector. */
  def *(that: Alt): Repr

  /** The component-wise division of this vector by another vector. */
  def /(that: Alt): Repr

  /** The addition of a scalar value to all components of this vector. */
  def +(s: Elem): Repr

  /** The subtraction of a scalar value from all components of this vector. */
  def -(s: Elem): Repr

  /** The product of a scalar value with all components of this vector. */
  def *(s: Elem): Repr

  /** The quotient of dividing all components of this vector by a scalar value. */
  def /(s: Elem): Repr

  /** Tests whether a predicate holds true for all components of this vector. */
  def forall(p: (Elem) => Boolean): Boolean

  /** Tests whether a predicate holds true for all of the corresponding components
    * of this and another vector.
    */
  def forall(that: Repr)(p: (Elem, Elem) => Boolean): Boolean

  /** Tests whether a predicate holds true for any component of this vector. */
  def forany(p: (Elem) => Boolean): Boolean

  /** Tests whether a predicate holds true for any of the corresponding components
    * of this and another vector.
    */
  def forany(that: Repr)(p: (Elem, Elem) => Boolean): Boolean

  /** Applies a function to all components of this vector.
    * @param p  The function that is applied for its side-effect only to every component.
    */
  def foreach(p: (Elem) => Unit): Unit

  /** Builds a new vector by applying a function to each component of this vector. */
  def map(p: (Elem) => Elem): Repr

  /** Applies a binary operator to a start value and all components of this vector, going
    * left to right.
    */
  def foldLeft[A](start: A)(f: (A, Elem) => A): A

  /** Applies a binary operator to a start value and all components of this vector, going
    * left to right.
    */
  def /:[A](start: A)(f: (A, Elem) => A): A

  /** Applies a binary operator to a start value and all components of this vector,
    * going right to left.
    */
  def foldRight[A](start: A)(f: (Elem, A) => A): A

  /** Applies a binary operator to a start value and all components of this vector,
    * going right to left.
    */
  def :\[A](start: A)(f: (Elem, A) => A): A

  /** Reduces the elements of this vector using the specified associative binary operator. */
  def reduce(p: (Elem, Elem) => Elem): Elem

  /** Builds a new vector by apply a function to the corresponding elements of this and another vector. */
  def compwise(that: Repr)(p: (Elem, Elem) => Elem): Repr

  /** Builds a new vector by apply a function to the corresponding elements of this and two other vectors. */
  def compwise(a: Repr, b: Repr)(p: (Elem, Elem, Elem) => Elem): Repr

  /** Builds a new vector who's components are the minimum of the corresponding components of this and another vector. */
  def min(that: Repr): Repr

  /** The minimum valued component of this vector. */
  def min: Elem

  /** Builds a new vector who's components are the maximum of the corresponding components of this and another vector. */
  def max(that: Repr): Repr

  /** The maximum valued component of this vector. */
  def max: Elem

  /** Builds a new vector who's components are clamped to be between the given upper and lower bounds. */
  def clamp(lower: Repr, upper: Repr): Repr
  
  /** Convert to a list of components. */
  def toList: List[Elem]

  /** Convert to an array of components. */
  def toArray: Array[Elem]

  /** Convert to a 4-dimensional vector. */
  def toVec4d: Vec4d

  /** Convert to a 3-dimensional vector. */
  def toVec3d: Vec3d

  /** Convert to a 2-dimensional vector. */
  def toVec2d: Vec2d

  /** Convert to a 3-dimensional position. */
  def toPos3d: Pos3d

  /** Convert to a 2-dimensional position. */
  def toPos2d: Pos2d

  /** Convert to a 3-dimensional index (ignoring the fractional part). */
  def toIndex3i: Index3i

  /** Convert to a 2-dimensional index (ignoring the fractional part). */
  def toIndex2i: Index2i
  
  /** Add the component values from this vector (as Chars) starting at the current buffer position to a native array. */
  def putNative(buf: CharBuffer)

  /** Add the component values from this vector (as Chars) starting at the current buffer position to a native array. */
  def >>>(buf: CharBuffer)
  
  /** Add the component values from this vector (as Shorts) starting at the current buffer position to a native array. */
  def putNative(buf: ShortBuffer)

  /** Add the component values from this vector (as Shorts) starting at the current buffer position to a native array. */
  def >>>(buf: ShortBuffer)
  
  /** Add the component values from this vector (as Ints) starting at the current buffer position to a native array. */
  def putNative(buf: IntBuffer)

  /** Add the component values from this vector (as Ints) starting at the current buffer position to a native array. */
  def >>>(buf: IntBuffer)
  
  /** Add the component values from this vector (as Longs) starting at the current buffer position to a native array. */
  def putNative(buf: LongBuffer)

  /** Add the component values from this vector (as Longs) starting at the current buffer position to a native array. */
  def >>>(buf: LongBuffer)
  
  /** Add the component values from this vector (as Floats) starting at the current buffer position to a native array. */
  def putNative(buf: FloatBuffer)

  /** Add the component values from this vector (as Floats) starting at the current buffer position to a native array. */
  def >>>(buf: FloatBuffer)
  
  /** Add the component values from this vector (as Doubles) starting at the current buffer position to a native array. */
  def putNative(buf: DoubleBuffer)

  /** Add the component values from this vector (as Doubles) starting at the current buffer position to a native array. */
  def >>>(buf: DoubleBuffer)
}

