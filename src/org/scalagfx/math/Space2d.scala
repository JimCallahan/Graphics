// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math

//import scala.math._


//--------------------------------------------------------------------------------------------------
//   S P A C E   2 D
//--------------------------------------------------------------------------------------------------

/** Companion object for Space2d. */
object Space2d 
{
  //------------------------------------------------------------------------------------------------
  //   C R E A T I O N                                                                      
  //------------------------------------------------------------------------------------------------

  /** Create a new spatial subdivision from bounding box corners.
    * 
    * @param bmin The bounding box minimum corner.
    * @param bmax The bounding box maximum corner.
    * @param size The number of cells in each dimension. */ 
  def apply(bmin: Pos2d, bmax: Pos2d, size: Index2i): Space2d = 
    new Space2d(BBox2d(bmin, bmax), size)

  /** Create a new spatial subdivision of a given bounding box.
    * 
    * @param bbox The bounding box. 
    * @param size The number of cells in each dimension. */ 
  def apply(bbox: BBox2d, size: Index2i): Space2d = 
    new Space2d(bbox, size)

  /** Create a new spatial subdivision of the unit bounding box.
    * 
    * @param size The number of cells in each dimension. */ 
  def apply(size: Index2i): Space2d = 
    new Space2d(BBox2d(Pos2d(0.0), Pos2d(1.0)), size)


  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether two spaces are within a given epsilon. */ 
  def equiv(a: Space2d, b: Space2d, epsilon: Double): Boolean = 
    a.equiv(b, epsilon) 
  
  /** The component-wise comparison of whether two spaces are within a type specific
    * epsilon. */ 
  def equiv(a: Space2d, b: Space2d): Boolean = 
    (a equiv b)
}

/** A regular subdivision of a axis aligned rectangular grid of space into cells.
  *
  * @constructor Create a new space.
  * @param bbox The bounding box of the space.
  * @param size The number of cells in each dimension. */ 
class Space2d(bbox: BBox2d, val size: Index2i) 
  extends BBox2d(bbox.bmin, bbox.bmax)
{
  if(size anyLte Index2i(0))
    throw new IllegalArgumentException(
      "The number of voxel cells in each dimension (size) must be a positive number!")

  //------------------------------------------------------------------------------------------------
  //   O P E R A T I O N S
  //------------------------------------------------------------------------------------------------

  /** The size of a individual cell. */ 
  def cellSize = range / size.toVec2d

  /** The world space bounding box of the cell with the given index. */ 
  def cellBounds(idx: Index2i): BBox2d =
    cellCoords(idx).map(position)

  /** The local coordinate bounding box of the cell with the given index. */ 
  def cellCoords(idx: Index2i): BBox2d = 
    BBox2d(idx.toPos2d / size.toVec2d, (idx+1).toPos2d / size.toVec2d)


  //------------------------------------------------------------------------------------------------

  /** The index of the voxel cell containing the given world space position. */ 
  def indexOf(pos: Pos2d): Index2i = 
    (coord(pos) * size.toVec2d).map(scala.math.floor _).toIndex2i

  /** The index of the voxel cell containing the given world space position and the index offsets
    * to the nearest neighboring cells.
    *
    * The index offset is an integer vector containing either 1 or -1 for each dimension indicating
    * the direction in index space of the nearest neighboring cell.
    * @return The index and index offset as a tuple. */ 
  def offsetsOf(pos: Pos2d): (Index2i, Index2i) = {
    val c = coord(pos)*size.toVec2d
    val w = c.map(scala.math.floor _)
    val o = c - w - Vec2d(0.5)
    (w.toIndex2i, o.map(scala.math.signum _).toIndex2i)
  }
  
  /** The index of the voxel cell containing the given world space position, index offsets to the
    * nearest neighboring cells and interpolation vector with these cells.
    *
    * The index offset is an integer vector containing either 1 or -1 for each dimension indicating
    * the direction in index space of the nearest neighboring cell.
    *
    * The interpolation vector is a floating point vector with values in the range [0.0, 1.0) which
    * provides the weighting to use when interpolating values of neighboring cells indiced by the
    * index and index offsets.
    * @return The index, index offset and interpolation vector as a tuple. */ 
  def interpOf(pos: Pos2d): (Index2i, Index2i, Vec2d) = {
    val c = coord(pos)*size.toVec2d
    val w = c.map(scala.math.floor _)
    val o = c - w - Vec2d(0.5)
    (w.toIndex2i, o.map(scala.math.signum _).toIndex2i, o.abs)
  }


  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether the given space in within a given epsilon of
    * this bounding box. */ 
  def equiv(that: Space2d, epsilon: Double): Boolean = 
    super.equiv(that, epsilon) && (size == that.size) 
  
  /** The component-wise comparison of whether the given space is within a type specific
    * epsilon of this bounding box. */ 
  def equiv(that: Space2d): Boolean = 
    super.equiv(that) && (size == that.size) 
  
  //------------------------------------------------------------------------------------------------

  /** Compares this bounding box to the specified value for equality. */
  override def equals(that: Any): Boolean = 
    that match {
      case that: Space2d => 
        (that canEqual this) && (bmin == that.bmin) && (bmax == that.bmax) && (size == that.size)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  override def canEqual(that: Any): Boolean = 
    that.isInstanceOf[Space2d]

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 
    53 * (47 * (43 + bmin.hashCode) + bmax.hashCode) + size.hashCode


  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a string representation. */
  override def toString() = 
    "Space2d(" + bmin + ", " + bmax + ", " + size + ")"
}

