// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math2

trait VecOps[@specialized(Double,Int) Elem, Repr <: Tuple]
  extends TupleOps[Elem, Repr, Repr]
{   
  /** The dot-product of this and another vector. */
  def dot (that: Repr): Elem
  
  /** The magnitude of this vector squared. */
  def magSq: Elem
  
  /** The magnitude of this vector. */
  def mag: Elem
   
  /** A vector of identical direction but unit length. */
  def normalized: Repr
}
