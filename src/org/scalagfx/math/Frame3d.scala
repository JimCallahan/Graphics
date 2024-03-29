// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math

import scala.math.{abs,sin,cos}
import java.nio._

//--------------------------------------------------------------------------------------------------
//   F R A M E   3 D                                                                         
//--------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------
//            Virtual Matrix (i, j)                                                         
//                                                                                          
//   | basisX.x basisY.x basisZ.x origin.x |                                                
//   | basisX.y basisY.y basisZ.y origin.y |                                                
//   | basisX.z basisY.z basisZ.z origin.z |                                                
//   | 0.0      0.0      0.0      1.0      |                                                
//--------------------------------------------------------------------------------------------------

/** Companion object for Frame3d. */
object Frame3d 
{
  //------------------------------------------------------------------------------------------------
  //   C R E A T I O N                                                                      
  //------------------------------------------------------------------------------------------------

  /** Create an identity coordinate frame at the world origin with unit basis vectors. */
  def apply() = 
    new Frame3d(Vec3d.unitX, Vec3d.unitY, Vec3d.unitZ, Pos3d.origin)
  
  /** Create a coordinate frame at the world origin with the given basis vectors. */
  def apply(basisX: Vec3d, basisY: Vec3d, basisZ: Vec3d) = 
    new Frame3d(basisX, basisY, basisZ, Pos3d.origin)
  
  /** Create an arbitrary coordinate frame. */
  def apply(basisX: Vec3d, basisY: Vec3d, basisZ: Vec3d, origin: Pos3d) = 
    new Frame3d(basisX, basisY, basisZ, origin)
  
  /** Create an arbitrary coordinate frame from a nested list (basis vectors followed by origin)
    * of the corresponding 4x4 matrix. */ 
  def apply(mx: List[List[Double]]) = 
    mx match {
      case List(List(bxx, bxy, bxz, 0.0), 
                List(byx, byy, byz, 0.0),  
                List(bzx, bzy, bzz, 0.0),  
                List( ox,  oy,  oz, 1.0)) => new Frame3d(Vec3d(bxx, bxy, bxz), 
                                                         Vec3d(byx, byy, byz),  
                                                         Vec3d(bzx, bzy, bzz),  
                                                         Pos3d( ox,  oy,  oz))
      case _ => throw new IllegalArgumentException(
        "The given nested list of values did not correspond to a legal 4x4 matrix!")
    }

  /** Create an arbitrary coordinate frame from a nested array (basis vectors followed by origin)
    * of the corresponding 4x4 matrix. */ 
  def apply(mx: Array[Array[Double]]) = 
    mx match {
      case Array(Array(bxx, bxy, bxz, 0.0), 
                 Array(byx, byy, byz, 0.0),  
                 Array(bzx, bzy, bzz, 0.0),  
                 Array( ox,  oy,  oz, 1.0)) => new Frame3d(Vec3d(bxx, bxy, bxz), 
                                                           Vec3d(byx, byy, byz),  
                                                           Vec3d(bzx, bzy, bzz),  
                                                           Pos3d( ox,  oy,  oz))
      case _ => throw new IllegalArgumentException(
        "The given nested array of values did not correspond to a legal 4x4 matrix!")
    }

  /** Create an arbitrary coordinate frame from a native array (basis vectors followed by origin)
    * of the corresponding 4x4 matrix. */ 
  def apply(mx: DoubleBuffer) = {
    if(mx.capacity != 16)
      throw new IllegalArgumentException(
        "The given native array did not contain (16) values!")
    mx.rewind
    (mx.get, mx.get, mx.get, mx.get, 
     mx.get, mx.get, mx.get, mx.get, 
     mx.get, mx.get, mx.get, mx.get, 
     mx.get, mx.get, mx.get, mx.get) match {
      case (bxx, bxy, bxz, 0.0, 
            byx, byy, byz, 0.0,  
            bzx, bzy, bzz, 0.0,  
             ox,  oy,  oz, 1.0) => new Frame3d(Vec3d(bxx, bxy, bxz), 
                                               Vec3d(byx, byy, byz),  
                                               Vec3d(bzx, bzy, bzz),  
                                               Pos3d( ox,  oy,  oz))
      case _ => throw new IllegalArgumentException(
        "The given native array of values did not correspond to a legal 4x4 matrix!")
    }
  }

  /** Create an arbitrary coordinate frame from a native array (basis vectors followed by origin)
    * of the corresponding 4x4 matrix of floats. */ 
  def apply(mx: FloatBuffer) = {
    if(mx.capacity != 16)
      throw new IllegalArgumentException(
        "The given native array did not contain (16) values!")
    mx.rewind
    (mx.get, mx.get, mx.get, mx.get, 
     mx.get, mx.get, mx.get, mx.get, 
     mx.get, mx.get, mx.get, mx.get, 
     mx.get, mx.get, mx.get, mx.get) match {
      case (bxx, bxy, bxz, 0.0f, 
            byx, byy, byz, 0.0f,  
            bzx, bzy, bzz, 0.0f,  
             ox,  oy,  oz, 1.0f) => new Frame3d(Vec3d(bxx.toDouble, bxy.toDouble, bxz.toDouble), 
                                                Vec3d(byx.toDouble, byy.toDouble, byz.toDouble),  
                                                Vec3d(bzx.toDouble, bzy.toDouble, bzz.toDouble),  
                                                Pos3d( ox.toDouble,  oy.toDouble,  oz.toDouble))
      case _ => throw new IllegalArgumentException(
        "The given native array of values did not correspond to a legal 4x4 matrix!")
    }
  }

  /** Create a new uniform scaling coordinate frame.
    * 
    * |  s  0.0 0.0 0.0 |
    * | 0.0  s  0.0 0.0 |
    * | 0.0 0.0  s  0.0 | 
    * | 0.0 0.0 0.0 1.0 | */
  def scale(s: Double) = 
    Frame3d(Vec3d.unitX*s, Vec3d.unitY*s, Vec3d.unitZ*s, Pos3d.origin)
  
  /** Create a new non-uniform scaling coordinate frame.
    *
    * |  x  0.0 0.0 0.0 | 
    * | 0.0  y  0.0 0.0 |
    * | 0.0 0.0  z  0.0 | 
    * | 0.0 0.0 0.0 1.0 | */
  def scale(v: Vec3d) = 
    Frame3d(Vec3d.unitX*v.x, Vec3d.unitY*v.y, Vec3d.unitZ*v.z, Pos3d.origin)
  
  /** Create a new non-uniform scaling coordinate frame.
    *
    * |  x  0.0 0.0 0.0 | 
    * | 0.0  y  0.0 0.0 |
    * | 0.0 0.0  z  0.0 | 
    * | 0.0 0.0 0.0 1.0 | */
  def scale(x: Double, y: Double, z: Double) = 
    Frame3d(Vec3d.unitX*x, Vec3d.unitY*y, Vec3d.unitZ*z, Pos3d.origin)
  
  /** Create a new translation coordinate frame.
    *
    * | 1.0 0.0 0.0  x  | 
    * | 0.0 1.0 0.0  y  |
    * | 0.0 0.0 1.0  z  | 
    * | 0.0 0.0 0.0 1.0 | */
  def translate(v: Vec3d) = 
    Frame3d(Vec3d.unitX, Vec3d.unitY, Vec3d.unitZ, v.toPos3d) 

  /** Create a new translation coordinate frame.
    *
    * | 1.0 0.0 0.0  x  | 
    * | 0.0 1.0 0.0  y  |
    * | 0.0 0.0 1.0  z  | 
    * | 0.0 0.0 0.0 1.0 | */
  def translate(x: Double, y: Double, z: Double) = 
    Frame3d(Vec3d.unitX, Vec3d.unitY, Vec3d.unitZ, Pos3d(x, y, z)) 
  
  /** Create a new rotation coordinate frame described by a counter-clockwise rotation of
    * the given number of radians about an arbitrary axis. */
  def rotate(axis: Vec3d, angle: Double) = {
    val s = sin(angle)
    val c = cos(angle) 								    
    val omc = 1.0 - c     
    Frame3d(Vec3d(omc * axis.x * axis.x + c,     					    
	          omc * axis.x * axis.y + s * axis.z, 				    
	          omc * axis.z * axis.x - s * axis.y),
            Vec3d(omc * axis.x * axis.y - s * axis.z,
                  omc * axis.y * axis.y + c,     					    
                  omc * axis.y * axis.z + s * axis.x),
            Vec3d(omc * axis.z * axis.x + s * axis.y, 				    
	          omc * axis.y * axis.z - s * axis.x, 				    
	          omc * axis.z * axis.z + c), 
            Pos3d.origin)
  }

  /** Create a new rotation coordinate frame described by a counter-clockwise rotation of
    * the given number of radians about the X axis. 
    *
    * | 1.0 0.0  0.0 0.0 | 
    * | 0.0 cos -sin 0.0 |
    * | 0.0 sin  cos 0.0 | 
    * | 0.0 0.0  0.0 1.0 | */ 
   def rotateX(angle: Double) = {
     val s = sin(angle)
     val c = cos(angle)
     Frame3d(Vec3d.unitX, Vec3d(0.0, c, s), Vec3d(0.0, -s, c), Pos3d.origin)
   }
  
  /** Create a new rotation coordinate frame described by a counter-clockwise rotation of
    * the given number of radians about the Y axis. 
    *
    * |  cos 0.0 sin 0.0 | 
    * |  0.0 1.0 0.0 0.0 |
    * | -sin 0.0 cos 0.0 | 
    * |  0.0 0.0 0.0 1.0 | */ 
   def rotateY(angle: Double) = {
     val s = sin(angle)
     val c = cos(angle)
     Frame3d(Vec3d(c, 0.0, -s), Vec3d.unitY, Vec3d(s, 0.0, c), Pos3d.origin)
   }

  /** Create a new rotation coordinate frame described by a counter-clockwise rotation of
    * the given number of radians about the Z axis. 
    *
    * | cos -sin 0.0 0.0 | 
    * | sin  cos 0.0 0.0 | 
    * | 0.0  1.0 1.0 0.0 |
    * | 0.0  0.0 0.0 1.0 | */ 
   def rotateZ(angle: Double) = {
     val s = sin(angle)
     val c = cos(angle)
     Frame3d(Vec3d(c, s, 0.0), Vec3d(-s, c, 0.0), Vec3d.unitZ, Pos3d.origin)
   }

  
  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether two coordinate frames are within a given
    * epsilon. */ 
  def equiv(a: Frame3d, b: Frame3d, epsilon: Double): Boolean = 
    a.equiv(b, epsilon) 
  
  /** The component-wise comparison of whether two coordinate frames are within a type
    * specific epsilon. */ 
  def equiv(a: Frame3d, b: Frame3d): Boolean = 
    (a equiv b)
}


/** An immutable coordinate frame defined by three basis vectors and an origin. */ 
class Frame3d(val basisX: Vec3d, val basisY: Vec3d, val basisZ: Vec3d, val origin: Pos3d) 
  extends MatrixLike
{
  //------------------------------------------------------------------------------------------------
  //   U N A R Y   O P S                                                                    
  //------------------------------------------------------------------------------------------------

  /** The number of dimensions. */
  val dimens = 4


  //------------------------------------------------------------------------------------------------
  //   O P E R A T O R S                                                                    
  //------------------------------------------------------------------------------------------------

  /** Transform a point in THIS coordinate frame to the identity (world space) coordinate 
    * frame.
    *
    * Equivalent to post-multiplying a column vector by the basis matrix and offset by
    * origin.*/
  def xform(p: Pos3d) = 
    origin + basisX*p.x + basisY*p.y + basisZ*p.z
  
  /** Transform a point in THIS coordinate frame to the identity (world space) coordinate 
    * frame.
    *
    * Equivalent to post-multiplying a column vector by the basis matrix and offset by
    * origin.*/
  def * (p: Pos3d) = xform(p)
  
  /** Transform a point in THIS coordinate frame to the identity (world space) coordinate 
    * frame.
    *
    * Equivalent to post-multiplying a column vector by the basis matrix. */
  def xform(v: Vec3d) = 
    basisX*v.x + basisY*v.y + basisZ*v.z 
  
  /** Transform a point in THIS coordinate frame to the identity (world space) coordinate 
    * frame.
    *
    * Equivalent to post-multiplying a column vector by the basis matrix. */
  def * (v: Vec3d) = xform(v)
  
   
  /** Concatenate (multiply) a coordinate frame (on the right) with this coordinate frame. */ 
  def concat(that: Frame3d) = 
    Frame3d(xform(that.basisX), xform(that.basisY), xform(that.basisZ), xform(that.origin))
   
  /** Concatenate (multiply) a coordinate frame (on the right) with this coordinate frame. */ 
  def * (that: Frame3d) = concat(that) 
   

  /** Find the inverse (if possible) of this coordinate frame.
   *
   * The inverse being that coordinate frame that transforms points from the world (identity)
   * coordinate frame to this one. */
  def inverse(): Option[Frame3d] = {

    /* lookup the column vector with the given index */ 
    def getColumn(fr: Frame3d, j: Int) = 
      j match {
        case 0 => fr.basisX
        case 1 => fr.basisY
        case 2 => fr.basisZ
      }

    /* compute the inverse of the basis 3x3 matrix */ 
    val inv33 = {
      def f(act: Int, cs: Frame3d, inv: Frame3d): Option[(Frame3d, Frame3d)] = 
        act match {
          case 3 => Some(cs, inv)
          case _ => {
            /* find largest pivot value and index in active column at or below active row */
            val (pivot, pi) = {
              def g(i: Int, p: Double, pi: Int, ls: List[Double]): (Double, Int) = 
                ls match {
                  case d :: ds => 
                    if(abs(d) > abs(p)) g(i+1, d, act+i, ds) 
                    else g(i+1, p, pi, ds)
                  case Nil => (p, pi)
                }

              g(0, 0.0, act, getColumn(cs, act).toList.drop(act)) 
            }
            
            /* if the pivot is zero (or nearly so), it can't be inverted! */ 	
            if(Scalar.equiv(pivot, 0.0)) 
              None
            else {		  
              /* swap the pivot row with the active row (if they are different) */ 
              val (cs2, inv2) = 
                if(pi > act) (cs.rowOpI(pi, act), inv.rowOpI(pi, act))
                else (cs, inv)

              /* normalize the active row by multiplying it by 1/pivot */ 
              val (cs3, inv3) = 
                (cs2.rowOpII(act, 1.0 / pivot), inv2.rowOpII(act, 1.0 / pivot))

              /* subtract the proper multiple of the active row from each of the other rows
                 so that they have zero's in the pivot column */   
              val (cs4, inv4) = {		
                def g(i: Int, cs5: Frame3d, inv5: Frame3d): (Frame3d, Frame3d) = 
                  i match {
                    case 3 => (cs5, inv5)
                    case _ if(i != act) => 
                      val scale = getColumn(cs5, act)(i) * -1.0
                      if(!Scalar.equiv(scale, 0.0)) 
                        g(i+1, cs5.rowOpIII(i, act, scale), inv5.rowOpIII(i, act, scale))
                      else 
                        g(i+1, cs5, inv5)
                    case _ => g(i+1, cs5, inv5)
                  }

                g(0, cs3, inv3)
              }

              /* process next row... */ 
              f(act+1, cs4, inv4)
            }
          }
        }

      f(0, this, Frame3d())
    }		
						  
    /* handle the last (virtual) row:							  
        rowOpI can be skipped since this is the last row.
        rowOpII can be skipped since the pivot is 1 by definition.
        rowOpIII can be simplified since the last row is all zeros except 		  
          for the last column which is one -- this means that only the last 		  
	  column of the other rows need to be altered. */ 	
    inv33 match {
      case Some(Pair(cs, inv)) => 
        Some(Frame3d(inv.basisX, inv.basisY, inv.basisZ, (inv.origin - cs.origin).toPos3d))
      case None => None
    }
  }
	
								   
  //------------------------------------------------------------------------------------------------
  //   R O W   O P S                                                                        
  //------------------------------------------------------------------------------------------------
  
  /** Create a coordinate frame in which two virtual matrix rows have been exchanged.
    *
    * @param i1 The index of the row to swap.
    * @param i2 The index of the row to swap. */
  def rowOpI(i1: Int, i2: Int) = 
    if(i1 == i2) this
    else Frame3d(basisX.newComp(i1, basisX(i2)).newComp(i2, basisX(i1)), 
                 basisY.newComp(i1, basisY(i2)).newComp(i2, basisY(i1)), 
                 basisZ.newComp(i1, basisZ(i2)).newComp(i2, basisZ(i1)), 
                 origin.newComp(i1, origin(i2)).newComp(i2, origin(i1)))

  /** Create a coordinate frame in which a given virtual matrix row is scaled by a
    * constant factor.
    *
    * @param i1 The index of the row to scale.
    * @param scale The scaling factor. */ 
  def rowOpII(i: Int, scale: Double) = 
    Frame3d(basisX.newComp(i, basisX(i) * scale),
            basisY.newComp(i, basisY(i) * scale),
            basisZ.newComp(i, basisZ(i) * scale),
            origin.newComp(i, origin(i) * scale))

  /** Create a coordinate frame in which a multiple of one virtual matrix row is summed
    * with another row.
    * 
    * @param i1 The index of the row to change.
    * @param i2 The index of the row to scale and sum.
    * @param scale The scaling factor. */
  def rowOpIII(i1: Int, i2: Int, scale: Double) = 
    Frame3d(basisX.newComp(i1, basisX(i1) + basisX(i2)*scale), 
            basisY.newComp(i1, basisY(i1) + basisY(i2)*scale), 
            basisZ.newComp(i1, basisZ(i1) + basisZ(i2)*scale), 
            origin.newComp(i1, origin(i1) + origin(i2)*scale))
  
  
  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Compares this position to the specified value for equality. */
  override def equals(that: Any): Boolean = 
    that match {
      case that: Frame3d => 
        (that canEqual this) &&
        (basisX == that.basisX) && (basisY == that.basisY) && (basisZ == that.basisZ) &&
        (origin == that.origin)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean = 
    that.isInstanceOf[Frame3d]

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 
    53 * (47 * (43 * (41 + basisX.hashCode) + basisY.hashCode) + basisZ.hashCode) + 
      origin.hashCode
  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether the given coordinate frame in within a given
    * epsilon of this coordinate frame. */ 
  def equiv(that: Frame3d, epsilon: Double): Boolean = 
    forall(that)(Scalar.equiv(_, _, epsilon))
  
  /** The component-wise comparison of whether the given coordinate frame is within a type
    * specific epsilon of this coordinate frame. */ 
  def equiv(that: Frame3d): Boolean = 
    forall(that)(Scalar.equiv(_, _))
  
  
  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                        
  //------------------------------------------------------------------------------------------------

  /** Lookup the cell of the corresponding 4x4 homogeneous matrix for this coordinate frame. */
  def apply(i: Int, j: Int) = 
    try {
      i match {
        case 0 | 1 | 2 =>
          j match {
            case 0 => basisX(i)
            case 1 => basisY(i)
            case 2 => basisZ(i)
            case 3 => origin(i)
            case _ => throw new IllegalArgumentException
          }
        case 3 => 
          j match {
            case 0 | 1 | 2 => 0.0
            case 3 => 1.0
            case _ => throw new IllegalArgumentException
          }
        case _ => throw new IllegalArgumentException
      }
    }
    catch {
      case _: IllegalArgumentException => 
        throw new IllegalArgumentException("Invalid index (" + i + ", " + j + ")!")
    }
  
  /** Tests whether the given predicate holds true for all components of this coordinate
    * frame. */ 
  def forall(p: (Double) => Boolean): Boolean = 
    basisX.forall(p) && basisY.forall(p) && basisZ.forall(p) && origin.forall(p) 

  /** Tests whether the given predicate holds true for all of the corresponding components
    * of this and the given coordinate frame. */ 
  def forall(that: Frame3d)(p: (Double, Double) => Boolean): Boolean = 
    basisX.forall(that.basisX)(p) && basisY.forall(that.basisY)(p) && 
    basisZ.forall(that.basisZ)(p) && origin.forall(that.origin)(p)

  /** Tests whether the given predicate holds true for any components of this coordinate
    * frame. */ 
  def forany(p: (Double) => Boolean): Boolean = 
    basisX.forany(p) && basisY.forany(p) && basisZ.forany(p) && origin.forany(p) 

  /** Tests whether the given predicate holds true for any of the corresponding components
    * of this and the given coordinate frame. */ 
  def forany(that: Frame3d)(p: (Double, Double) => Boolean): Boolean = 
    basisX.forany(that.basisX)(p) && basisY.forany(that.basisY)(p) && 
    basisZ.forany(that.basisZ)(p) && origin.forany(that.origin)(p)

  /** Applies a function to all components of this coordinate frame.
   *
   * @param f The function that is applied for its side-effect to every component. */
  def foreach(f: (Double) => Unit): Unit = { 
    basisX.foreach(f); basisY.foreach(f); basisZ.foreach(f); origin.foreach(f)
  }
  
  /** Builds a new coordinate frame by applying a function to each component of this
    * coordinate frame. */
  def map(f: (Double) => Double): Frame3d = 
    Frame3d(basisX.map(f), basisY.map(f), basisZ.map(f), origin.map(f))


  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a 4x4 matrix. */
  def toMatrix44d: Matrix44d = 
    Matrix44d(basisX.toVector4d, basisY.toVector4d, basisZ.toVector4d, origin.toVector4d) 

  /** Convert to a nested list (basis vectors followed by origin) of the corresponding 4x4
    * matrix. */ 
  def toList: List[List[Double]] =            
    List(List(basisX.x, basisX.y, basisX.z, 0.0), 
         List(basisY.x, basisY.y, basisY.z, 0.0), 
         List(basisZ.x, basisZ.y, basisZ.z, 0.0), 
         List(origin.x, origin.y, origin.z, 1.0))

  /** Convert to a nested array (basis vectors followed by origin) of the corresponding 4x4
    * matrix. */ 
  def toArray: Array[Array[Double]] = 
    Array(Array(basisX.x, basisX.y, basisX.z, 0.0), 
          Array(basisY.x, basisY.y, basisY.z, 0.0), 
          Array(basisZ.x, basisZ.y, basisZ.z, 0.0), 
          Array(origin.x, origin.y, origin.z, 1.0))

  /** Add the component values (basis vectors followed by origin) of the corresponding 4x4
    * matrix starting at the current position to given native array. */ 
  def putNative(buf: DoubleBuffer) {
    buf.put(basisX.x); buf.put(basisX.y); buf.put(basisX.z); buf.put(0.0) 
    buf.put(basisY.x); buf.put(basisY.y); buf.put(basisY.z); buf.put(0.0) 
    buf.put(basisZ.x); buf.put(basisZ.y); buf.put(basisZ.z); buf.put(0.0) 
    buf.put(origin.x); buf.put(origin.y); buf.put(origin.z); buf.put(1.0)
  }

  /** Add the component values (basis vectors followed by origin) of the corresponding 4x4
    * matrix starting at the current position to given native array of floats. */ 
  def putNativeFloats(buf: FloatBuffer) {
    buf.put(basisX.x.toFloat); buf.put(basisX.y.toFloat); buf.put(basisX.z.toFloat); buf.put(0.0f) 
    buf.put(basisY.x.toFloat); buf.put(basisY.y.toFloat); buf.put(basisY.z.toFloat); buf.put(0.0f) 
    buf.put(basisZ.x.toFloat); buf.put(basisZ.y.toFloat); buf.put(basisZ.z.toFloat); buf.put(0.0f) 
    buf.put(origin.x.toFloat); buf.put(origin.y.toFloat); buf.put(origin.z.toFloat); buf.put(1.0f)
  }

  /** Convert to a string representation. */
  override def toString() = 
    "Frame3d(" + basisX + ", " + basisY + ", " + basisZ + ", " + origin + ")"

}

