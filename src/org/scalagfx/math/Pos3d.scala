// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math

//--------------------------------------------------------------------------------------------------
//   P O S   3 D                                                                             
//--------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------
//  Supported Subset of Operations:                                                         
//                                                          
//    P + S -> P      P - S -> P      P * S -> P      P / V -> P       
//    V + S -> V      V - S -> V      V * S -> V      V / S -> V           
//
//       ---          P - P -> V         ---             ---                                 
//    P + V -> P      P - V -> P      P * V -> P      P / V -> P
//    V + V -> V      V - V -> V      V * V -> V      V / V -> V 
//                                                                                          
//  S = Scalar(Double), P = Position(Pos3d), V = Vector(Vec3d)                              
//--------------------------------------------------------------------------------------------------

/** Companion object for Pos3d. */
object Pos3d
{
  //------------------------------------------------------------------------------------------------
  //   C R E A T I O N                                                                      
  //------------------------------------------------------------------------------------------------

  /** Create a new position from components. */
  def apply(x: Double, y: Double, z: Double) = 
    new Pos3d(x, y, z)

  /** Create a new position in which all components are the same scalar value. */ 
  def apply(s: Double) = 
    new Pos3d(s, s, s) 
 
  /** The origin. */
  val origin: Pos3d = 
    Pos3d(0.0)
  

  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether two positions are within a given epsilon. */ 
  def equiv(a: Pos3d, b: Pos3d, epsilon: Double): Boolean = 
    a.equiv(b, epsilon) 
  
  /** The component-wise comparison of whether two positions are within a type specific
    * epsilon. */ 
  def equiv(a: Pos3d, b: Pos3d): Boolean = 
    (a equiv b)
  
  /** The component-wise minimum of two positions. */
  def min(a: Pos3d, b: Pos3d): Pos3d = 
    compwise(a, b, scala.math.min(_, _)) 
  
  /** The component-wise maximum of two positions. */
  def max(a: Pos3d, b: Pos3d): Pos3d = 
    compwise(a, b, scala.math.max(_, _)) 
  			
  
  //------------------------------------------------------------------------------------------------
  //   I N T E R P O L A T I O N                                                            
  //------------------------------------------------------------------------------------------------

  /** Linearly interpolate between two positions. */
  def lerp(a: Pos3d, b: Pos3d, t: Double): Pos3d = 
    compwise(a, b, Scalar.lerp(_, _, t)) 
  
  /** Smooth-step interpolate between two positions. */
  def smoothlerp(a: Pos3d, b: Pos3d, t: Double): Pos3d = 
    compwise(a, b, Scalar.smoothlerp(_, _, t)) 


  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                        
  //------------------------------------------------------------------------------------------------

  /** Create a position who's components are generated by applying the given binary operator
    * to each of the corresponding components of the given two positions. */ 
  def compwise(a: Pos3d, b: Pos3d, f: (Double, Double) => Double): Pos3d = 
    Pos3d(f(a.x, b.x), f(a.y, b.y), f(a.z, b.z))

}

/** An immutable 3-dimensional vector of Double element type used to represent a position in
  * space for use in computational geometry applications.
  *
  * This is not meant to be a general purpose vector, but rather to only defined the limited
  * set of operations which make geometric sense.  This allows Scala type checking to catch
  * many of the most common errors where scalars, vectors or positions are being accidently
  * used in a way that is geometrically meaningless. */ 
class Pos3d(val x: Double, val y: Double, val z: Double) extends Vector3dLike
{
  type Self = Pos3d

  //------------------------------------------------------------------------------------------------
  //   C O M P O N E N T   O P S                                                            
  //------------------------------------------------------------------------------------------------

  /** A copy of this position in which the X component has been replaced with the given
    * value. */ 
  def newX(v: Double): Pos3d = 
    Pos3d(v, y, z)
  
  /** A copy of this position in which Y component has been replaced with the given value. */ 
  def newY(v: Double): Pos3d = 
    Pos3d(x, v, z)
  
  /** A copy of this position in which Z component has been replaced with the given value. */ 
  def newZ(v: Double): Pos3d = 
    Pos3d(x, y, v)
  
  /** A copy of this position in which the component with the given index as been replaced. */
  def newComp(i: Int, v: Double) = 
    i match {
      case 0 => Pos3d(v, y, z)
      case 1 => Pos3d(x, v, z)
      case 2 => Pos3d(x, y, v)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }


  //------------------------------------------------------------------------------------------------
  //   U N A R Y   O P S                                                                    
  //------------------------------------------------------------------------------------------------

  /** A position reflected about the origin. */
  def negated: Pos3d = Pos3d(-x, -y, -z)

					    
  //------------------------------------------------------------------------------------------------
  //   O P E R A T O R S                                                                    
  //------------------------------------------------------------------------------------------------

  /** The addition of a scalar to all components of this position. */ 
  def + (scalar: Double): Pos3d = Pos3d(x+scalar, y+scalar, z+scalar) 

  /** The component-wise addition of a vector with this position. */ 
  def + (that: Vec3d): Pos3d = Pos3d(x+that.x, y+that.y, z+that.z)
  

  /** The subtraction of a scalar value to all components of this position. */ 
  def - (scalar: Double): Pos3d = Pos3d(x-scalar, y-scalar, z-scalar) 
  
  /** The component-wise subtraction a vector from this position. */ 
  def - (that: Vec3d): Pos3d = Pos3d(x-that.x, y-that.y, z-that.z)

  /** The vector from the given position to this position. */ 
  def - (that: Pos3d): Vec3d = Vec3d(x-that.x, y-that.y, z-that.z)


  /** The product of a scalar value with all components of this position. */ 
  def * (scalar: Double): Pos3d = Pos3d(x*scalar, y*scalar, z*scalar) 

  /** The component-wise multiplication of a vector with this position. */ 
  def * (that: Vec3d): Pos3d = Pos3d(x*that.x, y*that.y, z*that.z)
  

  /** The quotient of dividing all components of this position by a scalar value. */ 
  def / (scalar: Double): Pos3d = Pos3d(x/scalar, y/scalar, z/scalar) 

  /** The component-wise division of this position by a vector. */ 
  def / (that: Vec3d): Pos3d = Pos3d(x/that.x, y/that.y, z/that.z)
  

  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Compares this position to the specified value for equality. */
  override def equals(that: Any): Boolean = 
    that match {
      case that: Pos3d => 
        (that canEqual this) && (x == that.x) && (y == that.y) && (z == that.z)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean = 
    that.isInstanceOf[Pos3d]

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 
    47 * (43 * (41 + x.##) + y.##) + z.##


  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                        
  //------------------------------------------------------------------------------------------------

  /** Tests whether the given predicate holds true for all of the corresponding components
    * of this and the given position. */ 
  def forall(that: Pos3d)(p: (Double, Double) => Boolean): Boolean = 
    p(x, that.x) && p(y, that.y) && p(z, that.z)

  /** Tests whether the given predicate holds true for any of the corresponding components
    * of this and the given position. */ 
  def forany(that: Pos3d)(p: (Double, Double) => Boolean): Boolean = 
    p(x, that.x) || p(y, that.y) || p(z, that.z)

  /** Builds a new position by applying a function to each component of this position. */
  def map(f: (Double) => Double): Pos3d = 
    Pos3d(f(x), f(y), f(z))


  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a string representation. */
  override def toString() = 
    "Pos3d(%.2f, %.2f, %.2f)".format(x, y, z)
}

