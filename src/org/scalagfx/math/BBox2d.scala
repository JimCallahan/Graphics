// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math

import scala.util.Random

//--------------------------------------------------------------------------------------------------
//   B B O X   2 D                                                                           
//--------------------------------------------------------------------------------------------------

/** Companion object for BBox2d. */
object BBox2d 
{
  //------------------------------------------------------------------------------------------------
  //   C R E A T I O N                                                                      
  //------------------------------------------------------------------------------------------------

  /** Create a new bounding box given two corners. */
  def apply(bmin: Pos2d, bmax: Pos2d) = {
    val cmin = Pos2d.min(bmin, bmax)
    val cmax = Pos2d.max(bmin, bmax)
    new BBox2d(cmin, cmax) 
  }

  /** Create a new bounding box which contains the given points. */
  def apply(pts: Pos2d*): BBox2d = BBox2d(pts.toList)

  /** Create a new bounding box which contains the given list of points. */
  def apply(pts: List[Pos2d]): BBox2d = {
    pts match {
      case p :: ls => {
        val cmin = (p /: ls)(Pos2d.min(_, _))
        val cmax = (p /: ls)(Pos2d.max(_, _))
        new BBox2d(cmin, cmax) 
      }      
      case _ => BBox2d(Pos2d.origin, Pos2d.origin) 
    }
  }


  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether two bounding boxes are within a given epsilon. */ 
  def equiv(a: BBox2d, b: BBox2d, epsilon: Double): Boolean = 
    a.equiv(b, epsilon) 
  
  /** The component-wise comparison of whether two bounding boxes are within a type specific
    * epsilon. */ 
  def equiv(a: BBox2d, b: BBox2d): Boolean = 
    (a equiv b)
}

/** An immutable 2-dimensional bounding box Double element type.
  *
  * @constructor Create a new bounding box.
  * @param bmin The bounding box minimum corner.
  * @param bmax The bounding box maximum corner. */ 
class BBox2d(val bmin: Pos2d, val bmax: Pos2d) 
{				    
  //------------------------------------------------------------------------------------------------
  //   O P E R A T I O N S
  //------------------------------------------------------------------------------------------------

  /** The area enclosed by the bounding box. */ 
  def area: Double = 
    (1.0 /: range)(_ * _)

  /** Create a bounding box which grows the current bounding box to contain the given position. */ 
  def grow(p: Pos2d): BBox2d = 
    BBox2d(Pos2d.min(p, bmin), Pos2d.max(p, bmax))

  /** Create a bounding box which grows (or shrinks) the current bounding box by the given
    * amount. */ 
  def grow(v: Vec2d): BBox2d = 
    BBox2d(bmin-v, bmax+v) 
  
  /** The range of the bounding box. */ 
  def range: Vec2d = 
    bmax - bmin

  /** The center of the bounding box. */ 
  def center: Pos2d = 
    Pos2d.lerp(bmin, bmax, 0.5)

  /** Clamp the given point to the bounds of the box. */ 
  def clamp(p: Pos2d): Pos2d = 
    Pos2d.min(Pos2d.max(bmin, p), bmax)
				    
  /** The local coordinate of a world position. */ 
  def coord(pos: Pos2d): Pos2d = 
    ((pos - bmin) * (Vec2d.one / range)).toPos2d
    
  /** The world position of a local coordinate. */ 
  def position(coord: Pos2d): Pos2d = 
    bmin + (coord*range).toVec2d

  /** Generate a random position inside the bounding box. */ 
  def randomPos(): Pos2d = 
    bmin + Vec2d.random*range

  /** Generate a random position inside the bounding box.  
    *
    * @param gen The random number generator to use. */ 
  def randomPos(gen: Random): Pos2d = 
    bmin + Vec2d.random(gen)*range


  //------------------------------------------------------------------------------------------------
  //   P R E D I C A T E S
  //------------------------------------------------------------------------------------------------
  
  /** Whether the given point in inside of the bounding box. */ 
  def isInside(p: Pos2d): Boolean = 
    p.forall(bmin)(_ >= _) && p.forall(bmax)(_ <= _)

  /** Whether the given bounding box shares any volume with this bounding box. */ 
  def intersects(that: BBox2d): Boolean = {
    import scala.math.max
    !(Pos2d.max(bmin, that.bmin).forany(Pos2d.min(bmax, that.bmax))(_ > _))
  }
    

  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether the given bounding box in within a given epsilon of
    * this bounding box. */ 
  def equiv(that: BBox2d, epsilon: Double): Boolean = 
    bmin.equiv(that.bmin, epsilon) && bmax.equiv(that.bmax, epsilon)
  
  /** The component-wise comparison of whether the given bounding box is within a type specific
    * epsilon of this bounding box. */ 
  def equiv(that: BBox2d): Boolean = 
    (bmin equiv that.bmin) && (bmax equiv that.bmax)

  //------------------------------------------------------------------------------------------------

  /** Compares this bounding box to the specified value for equality. */
  override def equals(that: Any): Boolean = 
    that match {
      case that: BBox2d => 
        (that canEqual this) && (bmin == that.bmin) && (bmax == that.bmax)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean = 
    that.isInstanceOf[BBox2d]

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 
    43 * (41 + bmin.hashCode) + bmax.hashCode


  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                       
  //------------------------------------------------------------------------------------------------
  
  /** Tests whether the given predicate holds true for both bounding corners. */ 
  def forall(p: (Pos2d) => Boolean): Boolean = 
    p(bmin) && p(bmax)

  /** Tests whether the given predicate holds true for both of the corresponding bounding corners 
    * of this and the given bounding box. */ 
  def forall(that: BBox2d)(p: (Pos2d, Pos2d) => Boolean): Boolean = 
    p(bmin, that.bmin) && p(bmax, that.bmax)

  /** Tests whether the given predicate holds true for either bounding corners. */ 
  def forany(p: (Pos2d) => Boolean): Boolean = 
    p(bmin) || p(bmax)

  /** Tests whether the given predicate holds true for either of the corresponding bounding corners 
    * of this and the given bounding box. */ 
  def forany(that: BBox2d)(p: (Pos2d, Pos2d) => Boolean): Boolean = 
    p(bmin, that.bmin) || p(bmax, that.bmax)

  /** Applies a function to all bounding corners.
   *
   * @param p  The function that is applied for its side-effect to every corner.  */
  def foreach(p: (Pos2d) => Unit): Unit = { 
    p(bmin); p(bmax)
  }

  /** Builds a new bounding box by applying a function to each bounding corner. */ 
  def map(f: (Pos2d) => Pos2d): BBox2d = 
    BBox2d(f(bmin), f(bmax))


  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a 3-dimensional bounding box with zero size in Z-dimension. */ 
  def toBBox3d: BBox3d = 
    BBox3d(bmin.toPos3d, bmax.toPos3d)

  /** Convert to a string representation. */
  override def toString() = 
    "BBox2d(" + bmin + ", " + bmax + ")"

}

