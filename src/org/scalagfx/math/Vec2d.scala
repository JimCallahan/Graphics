// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math

import scala.util.Random

//--------------------------------------------------------------------------------------------------
//   V E C   2 D                                                                            
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
//  S = Scalar(Double), P = Position(Pos2d), V = Vector(Vec2d)                           
//--------------------------------------------------------------------------------------------------

/** Companion object for Vec2d. */
object Vec2d 
{
  //------------------------------------------------------------------------------------------------
  //   C R E A T I O N                                                                     
  //------------------------------------------------------------------------------------------------

  /** Create a new vector from components. */
  def apply(x: Double, y: Double) = 
    new Vec2d(x, y)

  /** Create a new vector in which all components are the same scalar value. */ 
  def apply(s: Double) = 
    new Vec2d(s, s) 
 

  /** A zero length vector. */
  val zero: Vec2d = 
    Vec2d(0.0)

  /** A vector with all components equal to (1.0). */
  val one: Vec2d = 
    Vec2d(1.0)

  /** A vector with all components equal to (0.5). */
  val half: Vec2d = 
    Vec2d(0.5)


  /** A unit length vector along the X-axis. */ 
  val unitX: Vec2d = 
    Vec2d(1.0, 0.0)

  /** A unit length vector along the Y-axis. */ 
  val unitY: Vec2d = 
    Vec2d(0.0, 1.0) 
  

  import scala.math.{cos,sin,Pi}

  /** A random vector with components in the range [0.0, 1.0) */ 
  def random: Vec2d = 
    Vec2d(scala.math.random, scala.math.random)

  /** A random vector with components in the range [0.0, 1.0)
    *
    * @param gen The random number generator to use. */ 
  def random(gen: Random): Vec2d = 
    Vec2d(gen.nextDouble, gen.nextDouble)

  /** A random direction of unit length. */ 
  def randomDir: Vec2d = {
    val t = scala.math.random * Pi * 2.0
    Vec2d(cos(t), sin(t))
  }

  /** A random direction of unit length. 
    *
    * @param gen The random number generator to use. */ 
  def randomDir(gen: Random): Vec2d = {
    val t = gen.nextDouble * Pi * 2.0
    Vec2d(cos(t), sin(t))
  }
  

  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether two vectors are within a given epsilon. */ 
  def equiv(a: Vec2d, b: Vec2d, epsilon: Double): Boolean = 
    a.equiv(b, epsilon) 
  
  /** The component-wise comparison of whether two vectors are within a type specific
    * epsilon. */ 
  def equiv(a: Vec2d, b: Vec2d): Boolean = 
    (a equiv b)
  
  /** The component-wise minimum of two vectors. */
  def min(a: Vec2d, b: Vec2d): Vec2d = 
    compwise(a, b, scala.math.min(_, _)) 
  
  /** The component-wise maximum of two vectors. */
  def max(a: Vec2d, b: Vec2d): Vec2d = 
    compwise(a, b, scala.math.max(_, _)) 
  			
  
  //------------------------------------------------------------------------------------------------
  //   I N T E R P O L A T I O N                                                           
  //------------------------------------------------------------------------------------------------

  /** Linearly interpolate between two vectors. */
  def lerp(a: Vec2d, b: Vec2d, t: Double): Vec2d = 
    compwise(a, b, Scalar.lerp(_, _, t)) 
  
  /** Smooth-step interpolate between two vectors. */
  def smoothlerp(a: Vec2d, b: Vec2d, t: Double): Vec2d = 
    compwise(a, b, Scalar.smoothlerp(_, _, t)) 


  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                        
  //------------------------------------------------------------------------------------------------

  /** Create a vector who's components are generated by applying the given binary operator
    * to each of the corresponding components of the given two vectors. */ 
  def compwise(a: Vec2d, b: Vec2d, f: (Double, Double) => Double): Vec2d = 
    Vec2d(f(a.x, b.x), f(a.y, b.y))
}

/** An immutable 2-dimensional vector of Double element type used to represent a direction
  * with magnitude for use in computational geometry applications.
  *
  * This is not meant to be a general purpose vector, but rather to only defined the limited
  * set of operations which make geometric sense.  This allows Scala type checking to catch
  * many of the most common errors where scalars, points or vectors are being accidently
  * used in a way that is geometrically meaningless. */
class Vec2d(val x: Double, val y: Double) extends Vector2dLike
{
  type Self = Vec2d
  
  //------------------------------------------------------------------------------------------------
  //   C O M P O N E N T   O P S                                                            
  //------------------------------------------------------------------------------------------------

  /** A copy of this vector in which the X component has been replaced with the given
    * value. */ 
  def newX(v: Double): Vec2d = 
    Vec2d(v, y)
  
  /** A copy of this vector in which the Y component has been replaced with the given
    * value. */ 
  def newY(v: Double): Vec2d = 
    Vec2d(x, v)
  
  /** A copy of this vector in which the component with the given index as been replaced. */
  def newComp(i: Int, v: Double) = 
    i match {
      case 0 => Vec2d(v, y)
      case 1 => Vec2d(x, v)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  
  //------------------------------------------------------------------------------------------------
  //   U N A R Y   O P S                                                                    
  //------------------------------------------------------------------------------------------------

  /** A vector of identical magnitude but opposite direction. */
  def negated: Vec2d = Vec2d(-x, -y)

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
  def + (scalar: Double): Vec2d = Vec2d(x+scalar, y+scalar) 

  /** The component-wise addition of another vector with this vector. */ 
  def + (that: Vec2d): Vec2d = Vec2d(x+that.x, y+that.y)
  

  /** The subtraction of a scalar value to all components of this vector. */ 
  def - (scalar: Double): Vec2d = Vec2d(x-scalar, y-scalar) 
  
  /** The component-wise subtraction of another vector from this vector. */ 
  def - (that: Vec2d): Vec2d = Vec2d(x-that.x, y-that.y)


  /** The product of a scalar value with all components of this vector. */ 
  def * (scalar: Double): Vec2d = Vec2d(x*scalar, y*scalar)

  /** The component-wise multiplication of another vector with this vector. */ 
  def * (that: Vec2d): Vec2d = Vec2d(x*that.x, y*that.y)


  /** The quotient of dividing all components of this vector by a scalar value. */ 
  def / (scalar: Double): Vec2d = Vec2d(x/scalar, y/scalar)

  /** The component-wise division of this vector by another vector. */ 
  def / (that: Vec2d): Vec2d = Vec2d(x/that.x, y/that.y)


  /** The dot-product of this and another vector. */
  def dot(that: Vec2d): Double = 
    x*that.x + y*that.y


  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean = 
    that match {
      case that: Vec2d => 
        (that canEqual this) && (x == that.x) && (y == that.y)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean = 
    that.isInstanceOf[Vec2d]

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 
    43 * (41 + x.##) + y.##


  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                        
  //------------------------------------------------------------------------------------------------

  /** Tests whether the given predicate holds true for all of the corresponding components
    * of this and the given vector. */ 
  def forall(that: Vec2d)(p: (Double, Double) => Boolean): Boolean = 
    p(x, that.x) && p(y, that.y)

  /** Tests whether the given predicate holds true for any of the corresponding components
    * of this and the given vector. */ 
  def forany(that: Vec2d)(p: (Double, Double) => Boolean): Boolean = 
    p(x, that.x) || p(y, that.y)

  /** Builds a new vector by applying a function to each component of this vector. */
  def map(f: (Double) => Double): Vec2d = 
    Vec2d(f(x), f(y))


  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a string representation. */
  override def toString() = 
    "Vec2d(%.2f, %.2f)".format(x, y)

}

