package org.scalagfx.math

import scala.util.Random

//--------------------------------------------------------------------------------------------------
//   V E C   3 D                                                                            
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

/** Companion object for Vec3d. */
object Vec3d 
{
  //------------------------------------------------------------------------------------------------
  //   C R E A T I O N                                                                     
  //------------------------------------------------------------------------------------------------

  /** Create a new vector from components. */
  def apply(x: Double, y: Double, z: Double) = 
    new Vec3d(x, y, z)

  /** Create a new vector in which all components are the same scalar value. */ 
  def apply(s: Double) = 
    new Vec3d(s, s, s) 
 

  /** A zero length vector. */
  val zero: Vec3d = 
    Vec3d(0.0)

  /** A vector with all components equal to (1.0). */
  val one: Vec3d = 
    Vec3d(1.0)

  /** A vector with all components equal to (0.5). */
  val half: Vec3d = 
    Vec3d(0.5)


  /** A unit length vector along the X-axis. */ 
  val unitX: Vec3d = 
    Vec3d(1.0, 0.0, 0.0)

  /** A unit length vector along the Y-axis. */ 
  val unitY: Vec3d = 
    Vec3d(0.0, 1.0, 0.0) 

  /** A unit length vector along the Z-axis. */ 
  val unitZ: Vec3d = 
    Vec3d(0.0, 0.0, 1.0) 
  

  import scala.math.{cos,sin,Pi}

  /** A random vector with components in the range [0.0, 1.0) */ 
  def random: Vec3d = 
    Vec3d(scala.math.random, scala.math.random, scala.math.random)

  /** A random vector with components in the range [0.0, 1.0)
    *
    * @param gen The random number generator to use. */ 
  def random(gen: Random): Vec3d = 
    Vec3d(gen.nextDouble, gen.nextDouble, gen.nextDouble)

  /** A random direction of unit length. */ 
  def randomDir: Vec3d = {
    // Generate a random point distributed evenly within the unit cube, try again if its outside 
    // the unit sphere and then project the chosen point onto the sphere.
    def f(): Vec3d = {
      val v = random*2.0 - 1.0
      val len = v.length
      if((len > 0.0) && (len < 1.0)) v * (1.0 / len)
      else f
    }
    f
  }

  /** A random direction of unit length. 
    *
    * @param gen The random number generator to use. */ 
  def randomDir(gen: Random): Vec3d = {
    // Generate a random point distributed evenly within the unit cube, try again if its outside 
    // the unit sphere and then project the chosen point onto the sphere.
    def f(): Vec3d = {
      val v = random(gen)*2.0 - 1.0
      val len = v.length
      if((len > 0.0) && (len < 1.0)) v * (1.0 / len)
      else f
    }
    f
  }


  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether two vectors are within a given epsilon. */ 
  def equiv(a: Vec3d, b: Vec3d, epsilon: Double): Boolean = 
    a.equiv(b, epsilon) 
  
  /** The component-wise comparison of whether two vectors are within a type specific
    * epsilon. */ 
  def equiv(a: Vec3d, b: Vec3d): Boolean = 
    (a equiv b)
  
  /** The component-wise minimum of two vectors. */
  def min(a: Vec3d, b: Vec3d): Vec3d = 
    compwise(a, b, scala.math.min(_, _)) 
  
  /** The component-wise maximum of two vectors. */
  def max(a: Vec3d, b: Vec3d): Vec3d = 
    compwise(a, b, scala.math.max(_, _)) 
  			
  
  //------------------------------------------------------------------------------------------------
  //   I N T E R P O L A T I O N                                                           
  //------------------------------------------------------------------------------------------------

  /** Linearly interpolate between two vectors. */
  def lerp(a: Vec3d, b: Vec3d, t: Double): Vec3d = 
    compwise(a, b, Scalar.lerp(_, _, t)) 
  
  /** Smooth-step interpolate between two vectors. */
  def smoothlerp(a: Vec3d, b: Vec3d, t: Double): Vec3d = 
    compwise(a, b, Scalar.smoothlerp(_, _, t)) 


  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                        
  //------------------------------------------------------------------------------------------------

  /** Create a vector who's components are generated by applying the given binary operator
    * to each of the corresponding components of the given two vectors. */ 
  def compwise(a: Vec3d, b: Vec3d, f: (Double, Double) => Double): Vec3d = 
    Vec3d(f(a.x, b.x), f(a.y, b.y), f(a.z, b.z))
}

/** An immutable 3-dimensional vector of Double element type used to represent a direction
  * with magnitude for use in computational geometry applications.
  *
  * This is not meant to be a general purpose vector, but rather to only defined the limited
  * set of operations which make geometric sense.  This allows Scala type checking to catch
  * many of the most common errors where scalars, points or vectors are being accidently
  * used in a way that is geometrically meaningless. */
class Vec3d(val x: Double, val y: Double, val z: Double) extends Vector3dLike
{
  type Self = Vec3d
  
  //------------------------------------------------------------------------------------------------
  //   C O M P O N E N T   O P S                                                            
  //------------------------------------------------------------------------------------------------

  /** A copy of this vector in which the X component has been replaced with the given
    * value. */ 
  def newX(v: Double): Vec3d = 
    Vec3d(v, y, z)
  
  /** A copy of this vector in which the Y component has been replaced with the given
    * value. */ 
  def newY(v: Double): Vec3d = 
    Vec3d(x, v, z)
  
  /** A copy of this vector in which the Z component has been replaced with the given
    * value. */ 
  def newZ(v: Double): Vec3d = 
    Vec3d(x, y, v)
  
  /** A copy of this vector in which the component with the given index as been replaced. */
  def newComp(i: Int, v: Double) = 
    i match {
      case 0 => Vec3d(v, y, z)
      case 1 => Vec3d(x, v, z)
      case 2 => Vec3d(x, y, v)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  
  //------------------------------------------------------------------------------------------------
  //   U N A R Y   O P S                                                                    
  //------------------------------------------------------------------------------------------------

  /** A vector of identical magnitude but opposite direction. */
  def negated: Vec3d = Vec3d(-x, -y, -z)

  /** The length (magnitude) of this vector squared. */
  def lengthSq: Double = dot(this)

  /** The length (magnitude) of this vector. */
  def length: Double = scala.math.sqrt(lengthSq)

  /** A vector of identical direction but unit length. */
  def normalized: Self = this * (1.0 / length) 

  					    
  //------------------------------------------------------------------------------------------------
  //   O P E R A T O R S                                                                    
  //------------------------------------------------------------------------------------------------

  /** The addition of a scalar value to all components of this vector. */ 
  def + (scalar: Double): Vec3d = Vec3d(x+scalar, y+scalar, z+scalar) 

  /** The component-wise addition of this vector with another vector. */ 
  def + (that: Vec3d): Vec3d = Vec3d(x+that.x, y+that.y, z+that.z)
  

  /** The subtraction of a scalar value from all components of this vector. */ 
  def - (scalar: Double): Vec3d = Vec3d(x-scalar, y-scalar, z-scalar) 
  
  /** The component-wise subtraction of another vector from this vector. */ 
  def - (that: Vec3d): Vec3d = Vec3d(x-that.x, y-that.y, z-that.z)


  /** The product of a scalar value with all components of this vector. */ 
  def * (scalar: Double): Vec3d = Vec3d(x*scalar, y*scalar, z*scalar) 

  /** The component-wise multiplication of this vector with another vector. */ 
  def * (that: Vec3d): Vec3d = Vec3d(x*that.x, y*that.y, z*that.z)


  /** The quotient of dividing all components of this vector by a scalar value. */ 
  def / (scalar: Double): Vec3d = Vec3d(x/scalar, y/scalar, z/scalar) 

  /** The component-wise division of this vector by another vector. */ 
  def / (that: Vec3d): Vec3d = Vec3d(x/that.x, y/that.y, z/that.z)


  /** The dot-product of this and another vector. */
  def dot(that: Vec3d): Double = 
    x*that.x + y*that.y + z*that.z

  /** The cross-product of this and another vector. */
  def cross(that: Vec3d): Vec3d = 
    Vec3d(y * that.z - z * that.y,					    
          z * that.x - x * that.z,				    
          x * that.y - y * that.x)		
  

  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean = 
    that match {
      case that: Vec3d => 
        (that canEqual this) && (x == that.x) && (y == that.y) && (z == that.z)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean = 
    that.isInstanceOf[Vec3d]

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 
    47 * (43 * (41 + x.##) + y.##) + z.##


  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                        
  //------------------------------------------------------------------------------------------------

  /** Tests whether the given predicate holds true for all of the corresponding components
    * of this and the given vector. */ 
  def forall(that: Vec3d)(p: (Double, Double) => Boolean): Boolean = 
    p(x, that.x) && p(y, that.y) && p(z, that.z)

  /** Tests whether the given predicate holds true for any of the corresponding components
    * of this and the given vector. */ 
  def forany(that: Vec3d)(p: (Double, Double) => Boolean): Boolean = 
    p(x, that.x) || p(y, that.y) || p(z, that.z)

  /** Builds a new vector by applying a function to each component of this vector. */
  def map(f: (Double) => Double): Vec3d = 
    Vec3d(f(x), f(y), f(z))


  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a string representation. */
  override def toString() = 
    "Vec3d(%.2f, %.2f, %.2f)".format(x, y, z)

}

