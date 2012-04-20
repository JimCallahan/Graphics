// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math2

trait PosOps[@specialized(Double,Int) Elem, Pos <: Position, Vec <: Vector]
  extends TupleOps[Elem, Pos, Vec]
{   
  def - (that: Pos): Vec
}