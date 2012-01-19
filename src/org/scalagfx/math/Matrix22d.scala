package org.scalagfx.math

import scala.math.{abs,sin,cos}
import java.nio._

//--------------------------------------------------------------------------------------------------
//   M A T R I X   2 2 D                                                                    
//--------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------
//   | basisX.x basisY.x |                                                
//   | basisX.y basisY.y |                                                
//--------------------------------------------------------------------------------------------------

/** Companion object for Matrix22d. */
object Matrix22d 
{
  //------------------------------------------------------------------------------------------------
  //   C R E A T I O N                                                                      
  //------------------------------------------------------------------------------------------------

  /** Create an identity matrix. */
  def apply() = 
    new Matrix22d(Vector2d.unitX, Vector2d.unitY) 
  
  /** Create an arbitrary matrix from basis vectors. */
  def apply(basisX: Vector2d, basisY: Vector2d) = 
    new Matrix22d(basisX, basisY)
  
  /** Create an arbitrary 2x2 matrix from a nested list of elements (basis vectors XYZ). */
  def apply(mx: List[List[Double]]) = 
    mx match {
      case List(List(bxx, bxy), 
                List(byx, byy)) => new Matrix22d(Vector2d(bxx, bxy), 
                                                 Vector2d(byx, byy))  
      case _ => throw new IllegalArgumentException(
        "The given nested list of values did not correspond to a legal 2x2 matrix!")
    }

  /** Create an arbitrary 2x2 matrix from a nested array of elements (basis vectors XYZ). */
  def apply(mx: Array[Array[Double]]) = 
    mx match {
      case Array(Array(bxx, bxy), 
                 Array(byx, byy)) => new Matrix22d(Vector2d(bxx, bxy), 
                                                   Vector2d(byx, byy)) 
      case _ => throw new IllegalArgumentException(
        "The given nested array of values did not correspond to a legal 2x2 matrix!")
    }

  /** Create an arbitrary 2x2 matrix from a native array of elements (basis vectors XYZ). */
  def apply(mx: DoubleBuffer) = {
    if(mx.capacity != 4)
      throw new IllegalArgumentException(
        "The given native array did not contain (4) values!")
    mx.rewind
    (mx.get, mx.get, 
     mx.get, mx.get) match {
      case (bxx, bxy,  
            byx, byy) => new Matrix22d(Vector2d(bxx, bxy), 
                                       Vector2d(byx, byy)) 
      case _ => throw new IllegalArgumentException(
        "The given native array of values did not correspond to a legal 2x2 matrix!")
    }
  }

  /** Create an arbitrary 2x2 matrix from a native array of float elements (basis vectors XYZ). */
  def apply(mx: FloatBuffer) = {
    if(mx.capacity != 4)
      throw new IllegalArgumentException(
        "The given native array did not contain (4) values!")
    mx.rewind
    (mx.get, mx.get, 
     mx.get, mx.get) match {
      case (bxx, bxy,  
            byx, byy) => new Matrix22d(Vector2d(bxx.toDouble, bxy.toDouble), 
                                       Vector2d(byx.toDouble, byy.toDouble)) 
      case _ => throw new IllegalArgumentException(
        "The given native array of values did not correspond to a legal 2x2 matrix!")
    }
  }

  /** Create a new diagonal matrix.
    * 
    * | v.x 0.0 |
    * | 0.0 v.y | */
  def diagonal(v: Vector2d): Matrix22d = 
    Matrix22d(Vector2d.unitX*v.x, Vector2d.unitY*v.y)

  /** Create a new diagonal matrix.
    * 
    * |  x  0.0 |
    * | 0.0  y  | */
  def diagonal(x: Double, y: Double): Matrix22d = 
    diagonal(Vector2d(x, y)) 

  
  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether two matrices are within a given
    * epsilon. */ 
  def equiv(a: Matrix22d, b: Matrix22d, epsilon: Double): Boolean = 
    a.equiv(b, epsilon) 
  
  /** The component-wise comparison of whether two matrices are within a type
    * specific epsilon. */ 
  def equiv(a: Matrix22d, b: Matrix22d): Boolean = 
    (a equiv b)


  /** The component-wise minimum of two matrices. */
  def min(a: Matrix22d, b: Matrix22d): Matrix22d = 
    compwise(a, b, scala.math.min(_, _)) 
  
  /** The component-wise maximum of two matrices. */
  def max(a: Matrix22d, b: Matrix22d): Matrix22d = 
    compwise(a, b, scala.math.max(_, _)) 


  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                        
  //------------------------------------------------------------------------------------------------

  /** Create a matrix who's components are generated by applying the given binary operator
    * to each of the corresponding components of the given two matrices. */ 
  def compwise(a: Matrix22d, b: Matrix22d, f: (Double, Double) => Double): Matrix22d = 
    Matrix22d(Vector2d.compwise(a.basisX, b.basisX, f), 
              Vector2d.compwise(a.basisY, b.basisY, f)) 
}


/** An arbitrary 2x2 matrix of Double value.
  *
  * @constructor Create a new matrix.
  * @param basisX The basis vector for the X-dimension.
  * @param basisY The basis vector for the Y-dimension. */ 
class Matrix22d(val basisX: Vector2d, val basisY: Vector2d)
  extends MatrixLike
{
  //------------------------------------------------------------------------------------------------
  //   U N A R Y   O P S                                                                    
  //------------------------------------------------------------------------------------------------

  /** The number of dimensions. */
  val dimens = 2


  //------------------------------------------------------------------------------------------------
  //   O P E R A T O R S                                                                    
  //------------------------------------------------------------------------------------------------

  /** Post-multiplying a column vector by this matrix. */
  def xform(v: Vector2d) = 
    basisX*v.x + basisY*v.y
  
  /** Post-multiplying a column vector by this matrix. */
  def * (v: Vector2d) = xform(v)


  /** Concatenate (multiply) a matrix (on the right) with this matrix. */ 
  def concat(that: Matrix22d) = 
    Matrix22d(xform(that.basisX), xform(that.basisY))
    
  /** Concatenate (multiply) a matrix (on the right) with this matrix. */ 
  def * (that: Matrix22d) = concat(that)


  //------------------------------------------------------------------------------------------------
 
  /** The addition of a scalar value to all components of this matrix. */ 
  def + (scalar: Double): Matrix22d = 
    Matrix22d(basisX+scalar, basisY+scalar) 

  /** The subtraction of a scalar value from all components of this matrix. */ 
  def - (scalar: Double): Matrix22d = 
    Matrix22d(basisX-scalar, basisY-scalar)

  /** The product of a scalar value with all components of this matrix. */ 
  def * (scalar: Double): Matrix22d = 
    Matrix22d(basisX*scalar, basisY*scalar)

  /** The quotient of dividing all components of this matrix by a scalar value. */ 
  def / (scalar: Double): Matrix22d = 
    Matrix22d(basisX/scalar, basisY/scalar)


  //------------------------------------------------------------------------------------------------

  /** Find the transpose of this matrix. */
  def transpose: Matrix22d = 
    Matrix22d(Vector2d(basisX.x, basisY.x), 
              Vector2d(basisX.y, basisY.y))
	
  /** Find the minor of the given cell (column, row) of this matrix. 
    *
    * The minor is the determinant of the 1x1 matrix which remains when the row and column of the
    * given cell are removed from the original 2x2 matrix. */
  def minor(col: Int, row: Int): Double = 
    (col, row) match {
      case (0, 0) => basisY.y
      case (0, 1) => basisY.x
      case (1, 0) => basisX.y
      case (1, 1) => basisX.x
      case _ => throw new IllegalArgumentException(
        "Invalid column (" + col + ") or row (" + row + ")!")
    }
	
  /** Find the matrix of minors of this matrix. */
  def minors: Matrix22d = 
    Matrix22d(Vector2d(basisY.y, basisY.x), 
              Vector2d(basisX.y, basisX.x))

  /** Find the cofactor of the given cell (column, row) of this matrix.
    *
    * The cofactor of a cell is the minor in which the sign of the result is determined by
    * whether the sum of the column and row indices in the original matrix is even (unchanged) 
    * or odd (flipped). */
  def cofactor(col: Int, row: Int): Double = {
    val mm = minor(col, row) 
    if((col+row)%2 == 0) mm else -mm
  }

  /** Find the matrix of cofactors of this matrix. */
  def cofactors: Matrix22d = 
    Matrix22d(Vector2d( basisY.y, -basisY.x), 
              Vector2d(-basisX.y,  basisX.x))	
	
  /** Find the adjoint of this matrix.
    *
    * The adjoint is the transpose of the cofactors matrix. */
  def adjoint: Matrix22d = 
    cofactors.transpose
	
  /** Find the determinant of this matrix. */
  def determinant: Double = 
    basisX.x*basisY.y - basisY.x*basisX.y

  /** Whether the matrix has an inverse (is non-singular). */ 
  def isInvertible = 
    !Scalar.equiv(determinant, 0.0) 

  /** Find the inverse (if possible) of this matrix. */
  def inverse: Option[Matrix22d] = {
    val det = determinant 
    if(Scalar.equiv(det, 0.0)) None
    else Some(adjoint/det)
  }
	
  
  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Compares this position to the specified value for equality. */
  override def equals(that: Any): Boolean = 
    that match {
      case that: Matrix22d => 
        (that canEqual this) &&
        (basisX == that.basisX) && (basisY == that.basisY) 
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean = 
    that.isInstanceOf[Matrix22d]

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 
    43 * (41 + basisX.hashCode) + basisY.hashCode

  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether the given matrix in within a given
    * epsilon of this matrix. */ 
  def equiv(that: Matrix22d, epsilon: Double): Boolean = 
    forall(that)(Scalar.equiv(_, _, epsilon))
  
  /** The component-wise comparison of whether the given matrix is within a type
    * specific epsilon of this matrix. */ 
  def equiv(that: Matrix22d): Boolean = 
    forall(that)(Scalar.equiv(_, _))
  
  
  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                        
  //------------------------------------------------------------------------------------------------

  /** Lookup a the value of a given cell (column, row) of this matrix. */ 
  def apply(col: Int, row: Int) = {
    val b = col match { 
      case 0 => basisX
      case 1 => basisY
      case _ => throw new IllegalArgumentException("Invalid column (" + col + ")!")
    }
    row match {
      case 0|1=> b(row)
      case _ => throw new IllegalArgumentException("Invalid row (" + row + ")!")
    }
  }
    
  /** Tests whether the given predicate holds true for all components of this coordinate
    * frame. */ 
  def forall(p: (Double) => Boolean): Boolean = 
    basisX.forall(p) && basisY.forall(p)

  /** Tests whether the given predicate holds true for all of the corresponding components
    * of this and the given matrix. */ 
  def forall(that: Matrix22d)(p: (Double, Double) => Boolean): Boolean = 
    basisX.forall(that.basisX)(p) && basisY.forall(that.basisY)(p) 

  /** Tests whether the given predicate holds true for any components of this coordinate
    * frame. */ 
  def forany(p: (Double) => Boolean): Boolean = 
    basisX.forany(p) && basisY.forany(p)

  /** Tests whether the given predicate holds true for any of the corresponding components
    * of this and the given matrix. */ 
  def forany(that: Matrix22d)(p: (Double, Double) => Boolean): Boolean = 
    basisX.forany(that.basisX)(p) && basisY.forany(that.basisY)(p)

  /** Applies a function to all components of this matrix.
   *
   * @param f The function that is applied for its side-effect to every component. */
  def foreach(f: (Double) => Unit): Unit = { 
    basisX.foreach(f); basisY.foreach(f)
  }
  
  /** Builds a new matrix by applying a function to each component of this
    * matrix. */
  def map(f: (Double) => Double): Matrix22d = 
    Matrix22d(basisX.map(f), basisY.map(f))


  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a nested list of elements (basis vectors XY). */ 
  def toList: List[List[Double]] =            
    List(List(basisX.x, basisX.y), 
         List(basisY.x, basisY.y))

  /** Convert to a nested array of elements (basis vectors XY). */ 
  def toArray: Array[Array[Double]] = 
    Array(Array(basisX.x, basisX.y), 
          Array(basisY.x, basisY.y))

  /** Add the component values (basis vectors XY) starting at the current position to given
    * native array. */ 
  def putNative(buf: DoubleBuffer) {
    buf.put(basisX.x); buf.put(basisX.y)
    buf.put(basisY.x); buf.put(basisY.y)
  }

  /** Add the component values (basis vectors XY) starting at the current position to given
    * native array of floats. */ 
  def putNativeFloats(buf: FloatBuffer) {
    buf.put(basisX.x.toFloat); buf.put(basisX.y.toFloat)
    buf.put(basisY.x.toFloat); buf.put(basisY.y.toFloat)
  }

  /** Convert to a string representation. */
  override def toString() = 
    "Matrix22d(" + basisX + ", " + basisY + ")"

}

