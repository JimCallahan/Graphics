// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math2

/** Base trait for all coordinate frames. */
trait Frame

/** Frame component access. */
trait FrameAccess[@specialized(Double) Elem]
  extends Frame {
  /** Lookup a the value of a given cell (column, row) of this matrix. */
  def apply(col: Int, row: Int): Elem
  
  /** The number of dimensions. */
  val dimens: Int
}

/** A 2-dimensional coordinate frame. */
trait Frame2[@specialized(Double) Elem, Vec <: Vector, Pos <: Position]
  extends FrameAccess[Elem] {
  /** The X-basis vector. */
  val basisX: Vec

  /** The Y-basis vector. */
  val basisY: Vec

  /** The origin position. */
  val origin: Pos

  val dimens = 2
}

/** A 3-dimensional coordinate frame. */
trait Frame3[@specialized(Double) Elem, Vec <: Vector, Pos <: Position]
  extends FrameAccess[Elem] {
  /** The X-basis vector. */
  val basisX: Vec

  /** The Y-basis vector. */
  val basisY: Vec

  /** The Z-basis vector. */
  val basisZ: Vec

  /** The origin position. */
  val origin: Pos

  val dimens = 3
}