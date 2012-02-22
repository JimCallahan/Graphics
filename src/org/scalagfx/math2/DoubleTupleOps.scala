package org.scalagfx.math2

trait DoubleTupleOps[Repr <: Tuple]
{   
  /** Linearly interpolate between this and another vector. */
  def lerp(that: Repr, t: Double): Repr
  
  /** Smooth-step interpolate between this and another vector. */
  def smoothlerp(that: Repr, t: Double): Repr
}
