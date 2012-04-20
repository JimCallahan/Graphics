// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math

import scala.math.{abs,sin,cos}
import java.nio._

//--------------------------------------------------------------------------------------------------
//   M A T R I X   3 3 D                                                                    
//--------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------
//   | basisX.x basisY.x basisZ.x |                                                
//   | basisX.y basisY.y basisZ.y |                                                
//   | basisX.z basisY.z basisZ.z |                                                
//--------------------------------------------------------------------------------------------------

/** Companion object for Matrix33d. */
object Matrix33d 
{
  //------------------------------------------------------------------------------------------------
  //   C R E A T I O N                                                                      
  //------------------------------------------------------------------------------------------------

  /** Create an identity matrix. */
  def apply() = 
    new Matrix33d(Vector3d.unitX, Vector3d.unitY, Vector3d.unitZ) 
  
  /** Create an arbitrary matrix from basis vectors. */
  def apply(basisX: Vector3d, basisY: Vector3d, basisZ: Vector3d) = 
    new Matrix33d(basisX, basisY, basisZ)
  
  /** Create an arbitrary 3x3 matrix from a nested list of elements (basis vectors XYZ). */
  def apply(mx: List[List[Double]]) = 
    mx match {
      case List(List(bxx, bxy, bxz), 
                List(byx, byy, byz),  
                List(bzx, bzy, bzz)) => new Matrix33d(Vector3d(bxx, bxy, bxz), 
                                                      Vector3d(byx, byy, byz),  
                                                      Vector3d(bzx, bzy, bzz))  
      case _ => throw new IllegalArgumentException(
        "The given nested list of values did not correspond to a legal 3x3 matrix!")
    }

  /** Create an arbitrary 3x3 matrix from a nested array of elements (basis vectors XYZ). */
  def apply(mx: Array[Array[Double]]) = 
    mx match {
      case Array(Array(bxx, bxy, bxz), 
                 Array(byx, byy, byz),  
                 Array(bzx, bzy, bzz)) => new Matrix33d(Vector3d(bxx, bxy, bxz), 
                                                        Vector3d(byx, byy, byz),  
                                                        Vector3d(bzx, bzy, bzz)) 
      case _ => throw new IllegalArgumentException(
        "The given nested array of values did not correspond to a legal 3x3 matrix!")
    }

  /** Create an arbitrary 3x3 matrix from a native array of elements (basis vectors XYZ). */
  def apply(mx: DoubleBuffer) = {
    if(mx.capacity != 9)
      throw new IllegalArgumentException(
        "The given native array did not contain (9) values!")
    mx.rewind
    (mx.get, mx.get, mx.get, 
     mx.get, mx.get, mx.get, 
     mx.get, mx.get, mx.get) match {
      case (bxx, bxy, bxz,  
            byx, byy, byz,   
            bzx, bzy, bzz) => new Matrix33d(Vector3d(bxx, bxy, bxz), 
                                            Vector3d(byx, byy, byz),  
                                            Vector3d(bzx, bzy, bzz)) 
      case _ => throw new IllegalArgumentException(
        "The given native array of values did not correspond to a legal 3x3 matrix!")
    }
  }

  /** Create an arbitrary 3x3 matrix from a native array of float elements (basis vectors XYZ). */
  def apply(mx: FloatBuffer) = {
    if(mx.capacity != 9)
      throw new IllegalArgumentException(
        "The given native array did not contain (9) values!")
    mx.rewind
    (mx.get, mx.get, mx.get, 
     mx.get, mx.get, mx.get, 
     mx.get, mx.get, mx.get) match {
      case (bxx, bxy, bxz,  
            byx, byy, byz,   
            bzx, bzy, bzz) => new Matrix33d(Vector3d(bxx.toDouble, bxy.toDouble, bxz.toDouble), 
                                            Vector3d(byx.toDouble, byy.toDouble, byz.toDouble),  
                                            Vector3d(bzx.toDouble, bzy.toDouble, bzz.toDouble)) 
      case _ => throw new IllegalArgumentException(     
        "The given native array of values did not correspond to a legal 3x3 matrix!")
    }
  }

  /** Create a new diagonal matrix.
    * 
    * | v.x 0.0 0.0 |
    * | 0.0 v.y 0.0 |
    * | 0.0 0.0 v.z | */
  def diagonal(v: Vector3d): Matrix33d = 
    Matrix33d(Vector3d.unitX*v.x, Vector3d.unitY*v.y, Vector3d.unitZ*v.z)

  /** Create a new diagonal matrix.
    * 
    * |  x  0.0 0.0 |
    * | 0.0  y  0.0 |
    * | 0.0 0.0  z  | */
  def diagonal(x: Double, y: Double, z: Double): Matrix33d = 
    diagonal(Vector3d(x, y, z)) 

  
  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether two matrixs are within a given
    * epsilon. */ 
  def equiv(a: Matrix33d, b: Matrix33d, epsilon: Double): Boolean = 
    a.equiv(b, epsilon) 
  
  /** The component-wise comparison of whether two matrixs are within a type
    * specific epsilon. */ 
  def equiv(a: Matrix33d, b: Matrix33d): Boolean = 
    (a equiv b)


  /** The component-wise minimum of two matrices. */
  def min(a: Matrix33d, b: Matrix33d): Matrix33d = 
    compwise(a, b, scala.math.min(_, _)) 
  
  /** The component-wise maximum of two matrices. */
  def max(a: Matrix33d, b: Matrix33d): Matrix33d = 
    compwise(a, b, scala.math.max(_, _)) 


  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                        
  //------------------------------------------------------------------------------------------------

  /** Create a matrix who's components are generated by applying the given binary operator
    * to each of the corresponding components of the given two matrices. */ 
  def compwise(a: Matrix33d, b: Matrix33d, f: (Double, Double) => Double): Matrix33d = 
    Matrix33d(Vector3d.compwise(a.basisX, b.basisX, f), 
              Vector3d.compwise(a.basisY, b.basisY, f), 
              Vector3d.compwise(a.basisZ, b.basisZ, f)) 
}


/** An arbitrary 3x3 matrix of Double value. 
  *
  * @constructor Create a new matrix.
  * @param basisX The basis vector for the X-dimension.
  * @param basisY The basis vector for the Y-dimension. 
  * @param basisZ The basis vector for the Z-dimension. */ 
class Matrix33d(val basisX: Vector3d, val basisY: Vector3d, val basisZ: Vector3d)
  extends MatrixLike
{
  //------------------------------------------------------------------------------------------------
  //   U N A R Y   O P S                                                                    
  //------------------------------------------------------------------------------------------------

  /** The number of dimensions. */
  val dimens = 3


  //------------------------------------------------------------------------------------------------
  //   O P E R A T O R S                                                                    
  //------------------------------------------------------------------------------------------------

  /** Post-multiplying a column vector by this matrix. */
  def xform(v: Vector3d) = 
    basisX*v.x + basisY*v.y + basisZ*v.z
  
  /** Post-multiplying a column vector by this matrix. */
  def * (v: Vector3d) = xform(v)


  /** Concatenate (multiply) a matrix (on the right) with this matrix. */ 
  def concat(that: Matrix33d) = 
    Matrix33d(xform(that.basisX), xform(that.basisY), xform(that.basisZ))
    
  /** Concatenate (multiply) a matrix (on the right) with this matrix. */ 
  def * (that: Matrix33d) = concat(that)


  //------------------------------------------------------------------------------------------------
 
  /** The addition of a scalar value to all components of this matrix. */ 
  def + (scalar: Double): Matrix33d = 
    Matrix33d(basisX+scalar, basisY+scalar, basisZ+scalar) 

  /** The subtraction of a scalar value from all components of this matrix. */ 
  def - (scalar: Double): Matrix33d = 
    Matrix33d(basisX-scalar, basisY-scalar, basisZ-scalar) 

  /** The product of a scalar value with all components of this matrix. */ 
  def * (scalar: Double): Matrix33d = 
    Matrix33d(basisX*scalar, basisY*scalar, basisZ*scalar) 

  /** The quotient of dividing all components of this matrix by a scalar value. */ 
  def / (scalar: Double): Matrix33d = 
    Matrix33d(basisX/scalar, basisY/scalar, basisZ/scalar) 



  //------------------------------------------------------------------------------------------------

  /** Find the transpose of this matrix. */
  def transpose: Matrix33d = 
    Matrix33d(Vector3d(basisX.x, basisY.x, basisZ.x), 
              Vector3d(basisX.y, basisY.y, basisZ.y), 
              Vector3d(basisX.z, basisY.z, basisZ.z))
	
  /** Find the 2x2 sub-matrix obtained by deleting the given column and row. */
  def submatrix(col: Int, row: Int): Matrix22d = 
    (col, row) match {
      case (0, 0) => Matrix22d(Vector2d(basisY.y, basisY.z), 
                               Vector2d(basisZ.y, basisZ.z))

      case (0, 1) => Matrix22d(Vector2d(basisY.x, basisY.z), 
                               Vector2d(basisZ.x, basisZ.z))

      case (0, 2) => Matrix22d(Vector2d(basisY.x, basisY.y), 
                               Vector2d(basisZ.x, basisZ.y))

      case (1, 0) => Matrix22d(Vector2d(basisX.y, basisX.z), 
                               Vector2d(basisZ.y, basisZ.z))

      case (1, 1) => Matrix22d(Vector2d(basisX.x, basisX.z), 
                               Vector2d(basisZ.x, basisZ.z))

      case (1, 2) => Matrix22d(Vector2d(basisX.x, basisX.y), 
                               Vector2d(basisZ.x, basisZ.y))

      case (2, 0) => Matrix22d(Vector2d(basisX.y, basisX.z), 
                               Vector2d(basisY.y, basisY.z))

      case (2, 1) => Matrix22d(Vector2d(basisX.x, basisX.z), 
                               Vector2d(basisY.x, basisY.z))

      case (2, 2) => Matrix22d(Vector2d(basisX.x, basisX.y), 
                               Vector2d(basisY.x, basisY.y))

      case _ => throw new IllegalArgumentException(
        "Invalid column (" + col + ") or row (" + row + ")!")
    }

  /** Find the minor of the given cell (column, row) of this matrix.
    *
    * The minor is the determinant of the 2x2 matrix which remains when the row and column of the
    * given cell are removed from the original 3x3 matrix. */
  def minor(col: Int, row: Int): Double = 
    submatrix(col, row).determinant

  /** Find the matrix of minors of this matrix. */
  def minors: Matrix33d = 
    Matrix33d(Vector3d(minor(0, 0), minor(0, 1), minor(0, 2)), 
              Vector3d(minor(1, 0), minor(1, 1), minor(1, 2)), 
              Vector3d(minor(2, 0), minor(2, 1), minor(2, 2))) 
	
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
  def cofactors: Matrix33d = {
    val even = Vector3d(1.0, -1.0, 1.0) 
    val odd = even * -1.0
    val mm = minors
    Matrix33d(mm.basisX*even, mm.basisY*odd, mm.basisZ*even)
  }

  /** Find the adjoint of this matrix.
    *
    * The adjoint is the transpose of the cofactors matrix. */
  def adjoint: Matrix33d = 
    cofactors.transpose
	
  /** Find the determinant of this matrix. */
  def determinant: Double = 
    basisX.x*cofactor(0, 0) + basisX.y*cofactor(0, 1) + basisX.z*cofactor(0, 2)

  /** Whether the matrix has an inverse (is non-singular). */ 
  def isInvertible = 
    !Scalar.equiv(determinant, 0.0) 

  /** Find the inverse (if possible) of this matrix. */
  def inverse: Option[Matrix33d] = {
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
      case that: Matrix33d => 
        (that canEqual this) &&
        (basisX == that.basisX) && (basisY == that.basisY) && (basisZ == that.basisZ) 
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean = 
    that.isInstanceOf[Matrix33d]

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 
    47 * (43 * (41 + basisX.hashCode) + basisY.hashCode) + basisZ.hashCode

  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether the given matrix in within a given
    * epsilon of this matrix. */ 
  def equiv(that: Matrix33d, epsilon: Double): Boolean = 
    forall(that)(Scalar.equiv(_, _, epsilon))
  
  /** The component-wise comparison of whether the given matrix is within a type
    * specific epsilon of this matrix. */ 
  def equiv(that: Matrix33d): Boolean = 
    forall(that)(Scalar.equiv(_, _))
  
  
  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                        
  //------------------------------------------------------------------------------------------------

  /** Lookup a the value of a given cell (column, row) of this matrix. */ 
  def apply(col: Int, row: Int) = {
    val b = col match { 
      case 0 => basisX
      case 1 => basisY
      case 2 => basisZ
      case _ => throw new IllegalArgumentException("Invalid column (" + col + ")!")
    }
    row match {
      case 0|1|2 => b(row)
      case _ => throw new IllegalArgumentException("Invalid row (" + row + ")!")
    }
  }
  
  /** Tests whether the given predicate holds true for all components of this coordinate
    * frame. */ 
  def forall(p: (Double) => Boolean): Boolean = 
    basisX.forall(p) && basisY.forall(p) && basisZ.forall(p)

  /** Tests whether the given predicate holds true for all of the corresponding components
    * of this and the given matrix. */ 
  def forall(that: Matrix33d)(p: (Double, Double) => Boolean): Boolean = 
    basisX.forall(that.basisX)(p) && basisY.forall(that.basisY)(p) && 
    basisZ.forall(that.basisZ)(p) 

  /** Tests whether the given predicate holds true for any components of this coordinate
    * frame. */ 
  def forany(p: (Double) => Boolean): Boolean = 
    basisX.forany(p) && basisY.forany(p) && basisZ.forany(p)

  /** Tests whether the given predicate holds true for any of the corresponding components
    * of this and the given matrix. */ 
  def forany(that: Matrix33d)(p: (Double, Double) => Boolean): Boolean = 
    basisX.forany(that.basisX)(p) && basisY.forany(that.basisY)(p) && 
    basisZ.forany(that.basisZ)(p)

  /** Applies a function to all components of this matrix.
   *
   * @param f The function that is applied for its side-effect to every component. */
  def foreach(f: (Double) => Unit): Unit = { 
    basisX.foreach(f); basisY.foreach(f); basisZ.foreach(f)
  }
  
  /** Builds a new matrix by applying a function to each component of this
    * matrix. */
  def map(f: (Double) => Double): Matrix33d = 
    Matrix33d(basisX.map(f), basisY.map(f), basisZ.map(f))


  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a nested list of elements (basis vectors XYZ). */ 
  def toList: List[List[Double]] =            
    List(List(basisX.x, basisX.y, basisX.z), 
         List(basisY.x, basisY.y, basisY.z), 
         List(basisZ.x, basisZ.y, basisZ.z))

  /** Convert to a nested array of elements (basis vectors XYZ). */ 
  def toArray: Array[Array[Double]] = 
    Array(Array(basisX.x, basisX.y, basisX.z), 
          Array(basisY.x, basisY.y, basisY.z), 
          Array(basisZ.x, basisZ.y, basisZ.z))

  /** Add the component values (basis vectors XYZ) starting at the current position to given
    * native array. */ 
  def putNative(buf: DoubleBuffer) {
    buf.put(basisX.x); buf.put(basisX.y); buf.put(basisX.z) 
    buf.put(basisY.x); buf.put(basisY.y); buf.put(basisY.z) 
    buf.put(basisZ.x); buf.put(basisZ.y); buf.put(basisZ.z) 
  }

  /** Add the component values (basis vectors XYZ) starting at the current position to given
    * native array of floats. */ 
  def putNativeFloats(buf: FloatBuffer) {
    buf.put(basisX.x.toFloat); buf.put(basisX.y.toFloat); buf.put(basisX.z.toFloat) 
    buf.put(basisY.x.toFloat); buf.put(basisY.y.toFloat); buf.put(basisY.z.toFloat) 
    buf.put(basisZ.x.toFloat); buf.put(basisZ.y.toFloat); buf.put(basisZ.z.toFloat) 
  }

  /** Convert to a string representation. */
  override def toString() = 
    "Matrix33d(" + basisX + ", " + basisY + ", " + basisZ + ")"

}

