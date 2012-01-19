package org.scalagfx.math

import scala.util.Random

//--------------------------------------------------------------------------------------------------
//   B B O X   3 D                                                                           
//--------------------------------------------------------------------------------------------------

/** Companion object for BBox3d. */
object BBox3d 
{
  //------------------------------------------------------------------------------------------------
  //   C R E A T I O N                                                                      
  //------------------------------------------------------------------------------------------------

  /** Create a new bounding box given two corners. */
  def apply(bmin: Pos3d, bmax: Pos3d) = {
    val cmin = Pos3d.min(bmin, bmax)
    val cmax = Pos3d.max(bmin, bmax)
    new BBox3d(cmin, cmax) 
  }

  /** Create a new bounding box which contains the given points. */
  def apply(pts: Pos3d*): BBox3d = BBox3d(pts.toList) 

  /** Create a new bounding box which contains the given list of points. */
  def apply(pts: List[Pos3d]): BBox3d = {
    pts match {
      case p :: ls => {
        val cmin = (p /: ls)(Pos3d.min(_, _))
        val cmax = (p /: ls)(Pos3d.max(_, _))
        new BBox3d(cmin, cmax) 
      }      
      case _ => BBox3d(Pos3d.origin, Pos3d.origin) 
    }
  }


  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether two bounding boxes are within a given epsilon. */ 
  def equiv(a: BBox3d, b: BBox3d, epsilon: Double): Boolean = 
    a.equiv(b, epsilon) 
  
  /** The component-wise comparison of whether two bounding boxes are within a type specific
    * epsilon. */ 
  def equiv(a: BBox3d, b: BBox3d): Boolean = 
    (a equiv b)
}

/** An immutable 3-dimensional bounding box Double element type.
  *
  * @constructor Create a new bounding box.
  * @param bmin The bounding box minimum corner.
  * @param bmax The bounding box maximum corner. */ 
class BBox3d(val bmin: Pos3d, val bmax: Pos3d) 
{	    
  //------------------------------------------------------------------------------------------------
  //   O P E R A T I O N S
  //------------------------------------------------------------------------------------------------

  /** The volume enclosed by the bounding box. */ 
  def volume: Double = 
    (1.0 /: range)(_ * _)

  /** Create a bounding box which grows the current bounding box to contain the given position. */ 
  def grow(p: Pos3d): BBox3d = 
    BBox3d(Pos3d.min(p, bmin), Pos3d.max(p, bmax))

  /** Create a bounding box which grows (or shrinks) the current bounding box by the given
    * amount. */ 
  def grow(v: Vec3d): BBox3d = 
    BBox3d(bmin-v, bmax+v) 
  
  /** The range of the bounding box. */ 
  def range: Vec3d = 
    bmax - bmin

  /** The center of the bounding box. */ 
  def center: Pos3d = 
    Pos3d.lerp(bmin, bmax, 0.5)

  /** Clamp the given point to the bounds of the box. */ 
  def clamp(p: Pos3d): Pos3d = 
    Pos3d.min(Pos3d.max(bmin, p), bmax)
			
  /** The local coordinate of a world position. */ 
  def coord(pos: Pos3d): Pos3d = 
    ((pos - bmin) * (Vec3d.one / range)).toPos3d
    
  /** The world position of a local coordinate. */ 
  def position(coord: Pos3d): Pos3d = 
    bmin + (coord*range).toVec3d
    
  /** Generate a random position inside the bounding box. */ 
  def randomPos(): Pos3d = bmin + Vec3d.random*range

  /** Generate a random position inside the bounding box.  
    *
    * @param gen The random number generator to use. */ 
  def randomPos(gen: Random): Pos3d = 
    bmin + Vec3d.random(gen)*range


  //------------------------------------------------------------------------------------------------
  //   P R E D I C A T E S
  //------------------------------------------------------------------------------------------------
  
  /** Whether the given point in inside of the bounding box. */ 
  def isInside(p: Pos3d): Boolean = 
    p.forall(bmin)(_ >= _) && p.forall(bmax)(_ <= _)

  /** Whether the given bounding box shares any volume with this bounding box. */ 
  def intersects(that: BBox3d): Boolean = {
    import scala.math.max
    !(Pos3d.max(bmin, that.bmin).forany(Pos3d.min(bmax, that.bmax))(_ > _))
  }
    

  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether the given bounding box in within a given epsilon of
    * this bounding box. */ 
  def equiv(that: BBox3d, epsilon: Double): Boolean = 
    bmin.equiv(that.bmin, epsilon) && bmax.equiv(that.bmax, epsilon)
  
  /** The component-wise comparison of whether the given bounding box is within a type specific
    * epsilon of this bounding box. */ 
  def equiv(that: BBox3d): Boolean = 
    (bmin equiv that.bmin) && (bmax equiv that.bmax)
  
  //------------------------------------------------------------------------------------------------

  /** Compares this bounding box to the specified value for equality. */
  override def equals(that: Any): Boolean = 
    that match {
      case that: BBox3d => 
        (that canEqual this) && (bmin == that.bmin) && (bmax == that.bmax)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean = 
    that.isInstanceOf[BBox3d]

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 
    43 * (41 + bmin.hashCode) + bmax.hashCode.##


  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                       
  //------------------------------------------------------------------------------------------------
  
  /** Tests whether the given predicate holds true for both bounding corners. */ 
  def forall(p: (Pos3d) => Boolean): Boolean = 
    p(bmin) && p(bmax)

  /** Tests whether the given predicate holds true for both of the corresponding bounding corners 
    * of this and the given bounding box. */ 
  def forall(that: BBox3d)(p: (Pos3d, Pos3d) => Boolean): Boolean = 
    p(bmin, that.bmin) && p(bmax, that.bmax)

  /** Tests whether the given predicate holds true for either bounding corners. */ 
  def forany(p: (Pos3d) => Boolean): Boolean = 
    p(bmin) || p(bmax)

  /** Tests whether the given predicate holds true for either of the corresponding bounding corners 
    * of this and the given bounding box. */ 
  def forany(that: BBox3d)(p: (Pos3d, Pos3d) => Boolean): Boolean =
    p(bmin, that.bmin) || p(bmax, that.bmax)

  /** Applies a function to all bounding corners.
   *
   * @param p  The function that is applied for its side-effect to every corner.  */
  def foreach(p: (Pos3d) => Unit): Unit = { 
    p(bmin); p(bmax)
  }

  /** Builds a new bounding box by applying a function to each bounding corner. */ 
  def map(f: (Pos3d) => Pos3d): BBox3d = 
    BBox3d(f(bmin), f(bmax))


  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a 2-dimensional bounding box. */ 
  def toBBox2d: BBox2d = 
    BBox2d(bmin.toPos2d, bmax.toPos2d)

  /** Convert to a string representation. */
  override def toString() = 
    "BBox3d(" + bmin + ", " + bmax + ")"

}

