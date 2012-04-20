// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math2

import scala.math.{ abs, sin, cos }
import java.nio.{ FloatBuffer, DoubleBuffer }

/** An immutable coordinate frame defined by three basis vectors and an origin. */
trait FrameOps[@specialized(Double) Elem, Vec <: Vector, Pos <: Position, Repr <: Frame] {
  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean
  
  /** The component-wise comparison of whether the given coordinate frame in within a given
    * epsilon of this coordinate frame.
    */
  def equiv(that: Repr, epsilon: Double): Boolean

  /** The component-wise comparison of whether the given coordinate frame is within a type
    * specific epsilon of this coordinate frame.
    */
  def equiv(that: Repr): Boolean

  /** Transform a point in THIS coordinate frame to the identity (world space) coordinate
    * frame.
    *
    * Equivalent to post-multiplying a column vector by the basis matrix and offset by
    * origin.
    */
  def xform(p: Pos): Pos

  /** Transform a point in THIS coordinate frame to the identity (world space) coordinate
    * frame.
    *
    * Equivalent to post-multiplying a column vector by the basis matrix and offset by
    * origin.
    */
  def *(p: Pos): Pos

  /** Transform a direction in THIS coordinate frame to the identity (world space) coordinate
    * frame.
    *
    * Equivalent to post-multiplying a column vector by the basis matrix.
    */
  def xform(v: Vec): Vec

  /** Transform a direction in THIS coordinate frame to the identity (world space) coordinate
    * frame.
    *
    * Equivalent to post-multiplying a column vector by the basis matrix.
    */
  def *(v: Vec): Vec

  /** Concatenate (multiply) a coordinate frame (on the right) with this coordinate frame. */
  def concat(that: Repr): Repr

  /** Concatenate (multiply) a coordinate frame (on the right) with this coordinate frame. */
  def *(that: Repr): Repr

  /** Find the inverse (if possible) of this coordinate frame.
    *
    * The inverse being that coordinate frame that transforms points from the world (identity)
    * coordinate frame to this one.
    */
  def inverse(): Option[Repr]

  /** Create a coordinate frame in which two virtual matrix rows have been exchanged.
    *
    * @param i1 The index of the row to swap.
    * @param i2 The index of the row to swap.
    */
  def rowOpI(i1: Int, i2: Int): Repr

  /** Create a coordinate frame in which a given virtual matrix row is scaled by a
    * constant factor.
    *
    * @param i1 The index of the row to scale.
    * @param scale The scaling factor.
    */
  def rowOpII(i: Int, scale: Elem): Repr

  /** Create a coordinate frame in which a multiple of one virtual matrix row is summed
    * with another row.
    *
    * @param i1 The index of the row to change.
    * @param i2 The index of the row to scale and sum.
    * @param scale The scaling factor.
    */
  def rowOpIII(i1: Int, i2: Int, scale: Elem): Repr

  /** Tests whether the given predicate holds true for all components of this coordinate
    * frame.
    */
  def forall(p: (Elem) => Boolean): Boolean 

  /** Tests whether the given predicate holds true for all of the corresponding components
    * of this and the given coordinate frame.
    */
  def forall(that: Repr)(p: (Elem, Elem) => Boolean): Boolean

  /** Tests whether the given predicate holds true for any components of this coordinate
    * frame.
    */
  def forany(p: (Elem) => Boolean): Boolean

  /** Tests whether the given predicate holds true for any of the corresponding components
    * of this and the given coordinate frame.
    */
  def forany(that: Repr)(p: (Elem, Elem) => Boolean): Boolean
  
  /** Applies a function to all components of this coordinate frame.
    *
    * @param f The function that is applied for its side-effect to every component.
    */
  def foreach(f: (Elem) => Unit): Unit

  /** Builds a new coordinate frame by applying a function to each component of this
    * coordinate frame.
    */
  def map(f: (Elem) => Elem): Repr

  /** Convert to a nested list (basis vectors followed by origin) of the corresponding 4x4
    * matrix.
    */
  def toList: List[List[Elem]]

  /** Convert to a nested array (basis vectors followed by origin) of the corresponding 4x4
    * matrix.
    */
  def toArray: Array[Array[Elem]]

  /** Add the component values (as Floats, basis vectors followed by origin) of the corresponding 4x4
    * matrix starting at the current position to given native array.
    */
  def putNative(buf: FloatBuffer) 

  /** Add the component values (as Floats, basis vectors followed by origin) of the corresponding 4x4
    * matrix starting at the current position to given native array.
    */
  def >>>(buf: FloatBuffer) 

  /** Add the component values (as Doubles, basis vectors followed by origin) of the corresponding 4x4
    * matrix starting at the current position to given native array.
    */
  def putNative(buf: DoubleBuffer) 

  /** Add the component values (as Doubles, basis vectors followed by origin) of the corresponding 4x4
    * matrix starting at the current position to given native array.
    */
  def >>>(buf: DoubleBuffer) 
}

