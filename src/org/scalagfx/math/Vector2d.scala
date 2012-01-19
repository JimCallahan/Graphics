package org.scalagfx.math

//--------------------------------------------------------------------------------------------------
//   V E C T O R   2 D                                                                     
//--------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------
//  Supported Operations:                                                      
//                                                          
//    V + S -> V      V - S -> V      V * S -> V      V / S -> V           
//
//    V + V -> V      V - V -> V      V * V -> V      V / V -> V 
//                                                                                       
//  S = Scalar(Double), V = Vector(Vector2d)                           
//--------------------------------------------------------------------------------------------------

/** Companion object for Vector2d. */
object Vector2d 
{
  //------------------------------------------------------------------------------------------------
  //   C R E A T I O N                                                                     
  //------------------------------------------------------------------------------------------------

  /** Create a new vector with all components set to the given value. */
  def apply(s: Double) = 
    new Vector2d(s, s) 

  /** Create a new vector from components. */
  def apply(x: Double, y: Double) = 
    new Vector2d(x, y)


  /** A unit length vector along the X-axis. */ 
  val unitX: Vector2d = 
    Vector2d(1.0, 0.0)

  /** A unit length vector along the Y-axis. */ 
  val unitY: Vector2d = 
    Vector2d(0.0, 1.0)


  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether two vectors are within a given epsilon. */ 
  def equiv(a: Vector2d, b: Vector2d, epsilon: Double): Boolean = 
    a.equiv(b, epsilon) 
  
  /** The component-wise comparison of whether two vectors are within a type specific
    * epsilon. */ 
  def equiv(a: Vector2d, b: Vector2d): Boolean = 
    (a equiv b)
  
  /** The component-wise minimum of two vectors. */
  def min(a: Vector2d, b: Vector2d): Vector2d = 
    compwise(a, b, scala.math.min(_, _)) 
  
  /** The componentwise maximum of two vectors. */
  def max(a: Vector2d, b: Vector2d): Vector2d = 
    compwise(a, b, scala.math.max(_, _)) 
  			
  
  //------------------------------------------------------------------------------------------------
  //   I N T E R P O L A T I O N                                                           
  //------------------------------------------------------------------------------------------------

  /** Linearly interpolate between two vectors. */
  def lerp(a: Vector2d, b: Vector2d, t: Double): Vector2d = 
    compwise(a, b, Scalar.lerp(_, _, t)) 
  
  /** Smooth-step interpolate between two vectors. */
  def smoothlerp(a: Vector2d, b: Vector2d, t: Double): Vector2d = 
    compwise(a, b, Scalar.smoothlerp(_, _, t)) 


  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                        
  //------------------------------------------------------------------------------------------------

  /** Create a vector who's components are generated by applying the given binary operator
    * to each of the corresponding components of the given two vectors. */ 
  def compwise(a: Vector2d, b: Vector2d, f: (Double, Double) => Double): Vector2d = 
    Vector2d(f(a.x, b.x), f(a.y, b.y)) 
}

/** An immutable 3-dimensional vector of Double element type used to represent vectors
  * for use in computational geometry applications. */ 
class Vector2d(val x: Double, val y: Double) extends Vector2dLike
{
  type Self = Vector2d
  
  //------------------------------------------------------------------------------------------------
  //   C O M P O N E N T   O P S                                                            
  //------------------------------------------------------------------------------------------------

  /** A copy of this vector in which the X component has been replaced with the given
    * value. */ 
  def newX(v: Double): Vector2d = 
    Vector2d(v, y)
  
  /** A copy of this vector in which the Y component has been replaced with the given
    * value. */ 
  def newY(v: Double): Vector2d = 
    Vector2d(x, v)
  
  /** A copy of this vector in which the component with the given index as been replaced. */
  def newComp(i: Int, v: Double) = 
    i match {
      case 0 => Vector2d(v, y)
      case 1 => Vector2d(x, v)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  
  //------------------------------------------------------------------------------------------------
  //   U N A R Y   O P S                                                                    
  //------------------------------------------------------------------------------------------------

  /** A vector of identical magnitude but opposite direction. */
  def negated: Vector2d = Vector2d(-x, -y)

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
  def + (scalar: Double): Vector2d = Vector2d(x+scalar, y+scalar)

  /** The component-wise addition of this vector with another vector. */ 
  def + (that: Vector2d): Vector2d = Vector2d(x+that.x, y+that.y)
  

  /** The subtraction of a scalar value from all components of this vector. */ 
  def - (scalar: Double): Vector2d = Vector2d(x-scalar, y-scalar)
  
  /** The component-wise subtraction of another vector from this vector. */ 
  def - (that: Vector2d): Vector2d = Vector2d(x-that.x, y-that.y)


  /** The product of a scalar value with all components of this vector. */ 
  def * (scalar: Double): Vector2d = Vector2d(x*scalar, y*scalar)

  /** The component-wise multiplication of this vector with another vector. */ 
  def * (that: Vector2d): Vector2d = Vector2d(x*that.x, y*that.y)


  /** The quotient of dividing all components of this vector by a scalar value. */ 
  def / (scalar: Double): Vector2d = Vector2d(x/scalar, y/scalar)

  /** The component-wise division of this vector by another vector. */ 
  def / (that: Vector2d): Vector2d = Vector2d(x/that.x, y/that.y)


  /** The dot-product of this and another vector. */
  def dot(that: Vector2d): Double = 
    x*that.x + y*that.y

  

  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean = 
    that match {
      case that: Vector2d => 
        (that canEqual this) && (x == that.x) && (y == that.y)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean = 
    that.isInstanceOf[Vector2d]

  /** Returns a hash code value for the object. */
  override def hashCode: Int =
    43 * (41 + x.##) + y.##


  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                        
  //------------------------------------------------------------------------------------------------

  /** Tests whether the given predicate holds true for all of the corresponding components
    * of this and the given vector. */ 
  def forall(that: Vector2d)(p: (Double, Double) => Boolean): Boolean = 
    p(x, that.x) && p(y, that.y)

  /** Tests whether the given predicate holds true for any of the corresponding components
    * of this and the given vector. */ 
  def forany(that: Vector2d)(p: (Double, Double) => Boolean): Boolean = 
    p(x, that.x) || p(y, that.y)

  /** Builds a new vector by applying a function to each component of this vector. */
  def map(f: (Double) => Double): Vector2d = 
    Vector2d(f(x), f(y))


  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------
  
  /** Convert to a string representation. */
  override def toString() = 
    "Vector2d(%.2f, %.2f)".format(x, y)

}

