package org.scalagfx.math

import scala.math.{abs,sin,cos}
import java.nio._

//--------------------------------------------------------------------------------------------------
//   M A T R I X   4 4 D                                                                    
//--------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------
//   | basisX.x basisY.x basisZ.x basisW.x |                                                
//   | basisX.y basisY.y basisZ.y basisW.y |                                                
//   | basisX.z basisY.z basisZ.z basisW.z |                                                
//   | basisX.w basisY.w basisZ.w basisW.w |                                                
//--------------------------------------------------------------------------------------------------

/** Companion object for Matrix44d. */
object Matrix44d 
{
  //------------------------------------------------------------------------------------------------
  //   C R E A T I O N                                                                      
  //------------------------------------------------------------------------------------------------

  /** Create an identity matrix. */
  def apply() = 
    new Matrix44d(Vector4d.unitX, Vector4d.unitY, Vector4d.unitZ, Vector4d.unitW) 
  
  /** Create an arbitrary matrix from basis vectors. */
  def apply(basisX: Vector4d, basisY: Vector4d, basisZ: Vector4d, basisW: Vector4d) = 
    new Matrix44d(basisX, basisY, basisZ, basisW)
  
  /** Create an arbitrary 4x4 matrix from a nested list of elements (basis vectors XYZW). */
  def apply(mx: List[List[Double]]) = 
    mx match {
      case List(List(bxx, bxy, bxz, bxw), 
                List(byx, byy, byz, byw),  
                List(bzx, bzy, bzz, bzw),  
                List(bwx, bwy, bwz, bww)) => new Matrix44d(Vector4d(bxx, bxy, bxz, bxw), 
                                                           Vector4d(byx, byy, byz, byw),  
                                                           Vector4d(bzx, bzy, bzz, bzw),  
                                                           Vector4d(bwx, bwy, bwz, bww))  
      case _ => throw new IllegalArgumentException(
        "The given nested list of values did not correspond to a legal 4x4 matrix!")
    }

  /** Create an arbitrary 4x4 matrix from a nested array of elements (basis vectors XYZW). */
  def apply(mx: Array[Array[Double]]) = 
    mx match {
      case Array(Array(bxx, bxy, bxz, bxw), 
                 Array(byx, byy, byz, byw),  
                 Array(bzx, bzy, bzz, bzw),  
                 Array(bwx, bwy, bwz, bww)) => new Matrix44d(Vector4d(bxx, bxy, bxz, bxw), 
                                                             Vector4d(byx, byy, byz, byw),  
                                                             Vector4d(bzx, bzy, bzz, bzw),  
                                                             Vector4d(bwx, bwy, bwz, bww)) 
      case _ => throw new IllegalArgumentException(
        "The given nested array of values did not correspond to a legal 4x4 matrix!")
    }

  /** Create an arbitrary 4x4 matrix from a native array of elements (basis vectors XYZW). */
  def apply(mx: DoubleBuffer) = {
    if(mx.capacity != 16)
      throw new IllegalArgumentException(
        "The given native array did not contain (16) values!")
    mx.rewind
    (mx.get, mx.get, mx.get, mx.get, 
     mx.get, mx.get, mx.get, mx.get, 
     mx.get, mx.get, mx.get, mx.get, 
     mx.get, mx.get, mx.get, mx.get) match {
      case (bxx, bxy, bxz, bxw, 
            byx, byy, byz, byw,  
            bzx, bzy, bzz, bzw,  
            bwx, bwy, bwz, bww) => new Matrix44d(Vector4d(bxx, bxy, bxz, bxw), 
                                                 Vector4d(byx, byy, byz, byw),  
                                                 Vector4d(bzx, bzy, bzz, bzw),  
                                                 Vector4d(bwx, bwy, bwz, bww)) 
      case _ => throw new IllegalArgumentException(
        "The given native array of values did not correspond to a legal 4x4 matrix!")
    }
  }

  /** Create an arbitrary 4x4 matrix from a native array of float elements (basis vectors XYZW) */
  def apply(mx: FloatBuffer) = {
    if(mx.capacity != 16)
      throw new IllegalArgumentException(
        "The given native array did not contain (16) values!")
    mx.rewind
    (mx.get, mx.get, mx.get, mx.get, 
     mx.get, mx.get, mx.get, mx.get, 
     mx.get, mx.get, mx.get, mx.get, 
     mx.get, mx.get, mx.get, mx.get) match {
      case (bxx, bxy, bxz, bxw, 
            byx, byy, byz, byw,  
            bzx, bzy, bzz, bzw,  
            bwx, bwy, bwz, bww) => 
              new Matrix44d(Vector4d(bxx.toDouble, bxy.toDouble, bxz.toDouble, bxw.toDouble), 
                            Vector4d(byx.toDouble, byy.toDouble, byz.toDouble, byw.toDouble),  
                            Vector4d(bzx.toDouble, bzy.toDouble, bzz.toDouble, bzw.toDouble),  
                            Vector4d(bwx.toDouble, bwy.toDouble, bwz.toDouble, bww.toDouble)) 
      case _ => throw new IllegalArgumentException(
        "The given native array of values did not correspond to a legal 4x4 matrix!")
    }
  }

  /** Create a new diagonal matrix.
    * 
    * | v.x 0.0 0.0 0.0 |
    * | 0.0 v.y 0.0 0.0 |
    * | 0.0 0.0 v.z 0.0 | 
    * | 0.0 0.0 0.0 v.w | */
  def diagonal(v: Vector4d): Matrix44d = 
    Matrix44d(Vector4d.unitX*v.x, Vector4d.unitY*v.y, Vector4d.unitZ*v.z, Vector4d.unitW*v.w)

  /** Create a new diagonal matrix.
    * 
    * |  x  0.0 0.0 0.0 |
    * | 0.0  y  0.0 0.0 |
    * | 0.0 0.0  z  0.0 | 
    * | 0.0 0.0 0.0  w  | */
  def diagonal(x: Double, y: Double, z: Double, w: Double): Matrix44d = 
    diagonal(Vector4d(x, y, z, w)) 

  /** Create a new uniform scaling matrix.
    * 
    * |  s  0.0 0.0 0.0 |
    * | 0.0  s  0.0 0.0 |
    * | 0.0 0.0  s  0.0 | 
    * | 0.0 0.0 0.0 1.0 | */
  def scale(s: Double) = 
    diagonal(Vector4d(s, s, s, 1.0))
  
  /** Create a new non-uniform scaling matrix.
    *
    * |  x  0.0 0.0 0.0 | 
    * | 0.0  y  0.0 0.0 |
    * | 0.0 0.0  z  0.0 | 
    * | 0.0 0.0 0.0 1.0 | */
  def scale(v: Vector3d) = 
    diagonal(v.toVector4d)
  
  /** Create a new non-uniform scaling matrix.
    *
    * |  x  0.0 0.0 0.0 | 
    * | 0.0  y  0.0 0.0 |
    * | 0.0 0.0  z  0.0 | 
    * | 0.0 0.0 0.0 1.0 | */
  def scale(x: Double, y: Double, z: Double) = 
    diagonal(Vector4d(x, y, z, 1.0))
  
  /** Create a new translation matrix.
    *
    * | 1.0 0.0 0.0  x  | 
    * | 0.0 1.0 0.0  y  |
    * | 0.0 0.0 1.0  z  | 
    * | 0.0 0.0 0.0 1.0 | */
  def translate(v: Vector3d) = 
    Matrix44d(Vector4d.unitX, Vector4d.unitY, Vector4d.unitZ, v.toVector4d)

  /** Create a new translation matrix.
    *
    * | 1.0 0.0 0.0  x  | 
    * | 0.0 1.0 0.0  y  |
    * | 0.0 0.0 1.0  z  | 
    * | 0.0 0.0 0.0 1.0 | */
  def translate(x: Double, y: Double, z: Double) = 
    Matrix44d(Vector4d.unitX, Vector4d.unitY, Vector4d.unitZ, Vector4d(x, y, z, 1.0))
  
  /** Create a new rotation matrix described by a counter-clockwise rotation of
    * the given number of radians about an arbitrary axis. */
  def rotate(axis: Vector3d, angle: Double) = {
    val s = sin(angle)
    val c = cos(angle) 								    
    val omc = 1.0 - c     
    Matrix44d(Vector4d(omc * axis.x * axis.x + c,     					    
	               omc * axis.x * axis.y + s * axis.z, 				    
	               omc * axis.z * axis.x - s * axis.y, 
                       0.0),
              Vector4d(omc * axis.x * axis.y - s * axis.z,
                       omc * axis.y * axis.y + c,     					    
                       omc * axis.y * axis.z + s * axis.x, 
                       0.0),
              Vector4d(omc * axis.z * axis.x + s * axis.y, 				    
	               omc * axis.y * axis.z - s * axis.x, 				    
	               omc * axis.z * axis.z + c, 
                       0.0), 
              Vector4d.unitW)
  }

  /** Create a new rotation matrix described by a counter-clockwise rotation of
    * the given number of radians about the X axis. 
    *
    * | 1.0 0.0  0.0 0.0 | 
    * | 0.0 cos -sin 0.0 |
    * | 0.0 sin  cos 0.0 | 
    * | 0.0 0.0  0.0 1.0 | */ 
   def rotateX(angle: Double) = {
     val s = sin(angle)
     val c = cos(angle)
     Matrix44d(Vector4d.unitX, Vector4d(0.0, c, s, 0.0), 
               Vector4d(0.0, -s, c, 0.0), Vector4d.unitW)
   }
  
  /** Create a new rotation matrix described by a counter-clockwise rotation of
    * the given number of radians about the Y axis. 
    *
    * |  cos 0.0 sin 0.0 | 
    * |  0.0 1.0 0.0 0.0 |
    * | -sin 0.0 cos 0.0 | 
    * |  0.0 0.0 0.0 1.0 | */ 
   def rotateY(angle: Double) = {
     val s = sin(angle)
     val c = cos(angle)
     Matrix44d(Vector4d(c, 0.0, -s, 0.0), Vector4d.unitY, 
               Vector4d(s, 0.0, c, 0.0), Vector4d.unitW)
   }

  /** Create a new rotation matrix described by a counter-clockwise rotation of
    * the given number of radians about the Z axis. 
    *
    * | cos -sin 0.0 0.0 | 
    * | sin  cos 0.0 0.0 | 
    * | 0.0  1.0 1.0 0.0 |
    * | 0.0  0.0 0.0 1.0 | */ 
   def rotateZ(angle: Double) = {
     val s = sin(angle)
     val c = cos(angle)
     Matrix44d(Vector4d(c, s, 0.0, 0.0), Vector4d(-s, c, 0.0, 0.0), 
               Vector4d.unitZ, Vector4d.unitW)
   }

  
  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether two matrices are within a given
    * epsilon. */ 
  def equiv(a: Matrix44d, b: Matrix44d, epsilon: Double): Boolean = 
    a.equiv(b, epsilon) 
  
  /** The component-wise comparison of whether two matrices are within a type
    * specific epsilon. */ 
  def equiv(a: Matrix44d, b: Matrix44d): Boolean = 
    (a equiv b)


  /** The component-wise minimum of two matrices. */
  def min(a: Matrix44d, b: Matrix44d): Matrix44d = 
    compwise(a, b, scala.math.min(_, _)) 
  
  /** The component-wise maximum of two matrices. */
  def max(a: Matrix44d, b: Matrix44d): Matrix44d = 
    compwise(a, b, scala.math.max(_, _)) 


  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                        
  //------------------------------------------------------------------------------------------------

  /** Create a matrix who's components are generated by applying the given binary operator
    * to each of the corresponding components of the given two matrices. */ 
  def compwise(a: Matrix44d, b: Matrix44d, f: (Double, Double) => Double): Matrix44d = 
    Matrix44d(Vector4d.compwise(a.basisX, b.basisX, f), 
              Vector4d.compwise(a.basisY, b.basisY, f), 
              Vector4d.compwise(a.basisZ, b.basisZ, f), 
              Vector4d.compwise(a.basisW, b.basisW, f)) 
}


/** An arbitrary homogeneous 4x4 matrix of Double value. 
  *
  * @constructor Create a new matrix.
  * @param basisX The basis vector for the X-dimension.
  * @param basisY The basis vector for the Y-dimension. 
  * @param basisZ The basis vector for the Z-dimension.
  * @param basisW The basis vector for the W-dimension. */ 
class Matrix44d(val basisX: Vector4d, val basisY: Vector4d, 
                val basisZ: Vector4d, val basisW: Vector4d) 
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

  /** Post-multiplying a column vector by this matrix. */
  def xform(v: Vector4d) = 
    basisX*v.x + basisY*v.y + basisZ*v.z + basisW*v.w
  
  /** Post-multiplying a column vector by this matrix. */
  def * (v: Vector4d) = xform(v)


  /** Concatenate (multiply) a matrix (on the right) with this matrix. */ 
  def concat(that: Matrix44d) = 
    Matrix44d(xform(that.basisX), xform(that.basisY), xform(that.basisZ), xform(that.basisW))
    
  /** Concatenate (multiply) a matrix (on the right) with this matrix. */ 
  def * (that: Matrix44d) = concat(that)


  //------------------------------------------------------------------------------------------------
 
  /** The addition of a scalar value to all components of this matrix. */ 
  def + (scalar: Double): Matrix44d = 
    Matrix44d(basisX+scalar, basisY+scalar, basisZ+scalar, basisW+scalar) 

  /** The subtraction of a scalar value from all components of this matrix. */ 
  def - (scalar: Double): Matrix44d = 
    Matrix44d(basisX-scalar, basisY-scalar, basisZ-scalar, basisW-scalar) 

  /** The product of a scalar value with all components of this matrix. */ 
  def * (scalar: Double): Matrix44d = 
    Matrix44d(basisX*scalar, basisY*scalar, basisZ*scalar, basisW*scalar) 

  /** The quotient of dividing all components of this matrix by a scalar value. */ 
  def / (scalar: Double): Matrix44d = 
    Matrix44d(basisX/scalar, basisY/scalar, basisZ/scalar, basisW/scalar) 



  //------------------------------------------------------------------------------------------------

  /** Find the transpose of this matrix. */
  def transpose: Matrix44d = 
    Matrix44d(Vector4d(basisX.x, basisY.x, basisZ.x, basisW.x), 
              Vector4d(basisX.y, basisY.y, basisZ.y, basisW.y), 
              Vector4d(basisX.z, basisY.z, basisZ.z, basisW.z), 
              Vector4d(basisX.w, basisY.w, basisZ.w, basisW.w))
	
  /** Find the 3x3 sub-matrix obtained by deleting the given column and row. */
  def submatrix(col: Int, row: Int): Matrix33d = 
    (col, row) match {
      case (0, 0) => Matrix33d(Vector3d(basisY.y, basisY.z, basisY.w), 
                               Vector3d(basisZ.y, basisZ.z, basisZ.w), 
                               Vector3d(basisW.y, basisW.z, basisW.w)) 
        
      case (0, 1) => Matrix33d(Vector3d(basisY.x, basisY.z, basisY.w), 
                               Vector3d(basisZ.x, basisZ.z, basisZ.w), 
                               Vector3d(basisW.x, basisW.z, basisW.w)) 
      
      case (0, 2) => Matrix33d(Vector3d(basisY.x, basisY.y, basisY.w), 
                               Vector3d(basisZ.x, basisZ.y, basisZ.w), 
                               Vector3d(basisW.x, basisW.y, basisW.w)) 

      case (0, 3) => Matrix33d(Vector3d(basisY.x, basisY.y, basisY.z), 
                               Vector3d(basisZ.x, basisZ.y, basisZ.z), 
                               Vector3d(basisW.x, basisW.y, basisW.z)) 

      case (1, 0) => Matrix33d(Vector3d(basisX.y, basisX.z, basisX.w), 
                               Vector3d(basisZ.y, basisZ.z, basisZ.w), 
                               Vector3d(basisW.y, basisW.z, basisW.w)) 

      case (1, 1) => Matrix33d(Vector3d(basisX.x, basisX.z, basisX.w), 
                               Vector3d(basisZ.x, basisZ.z, basisZ.w), 
                               Vector3d(basisW.x, basisW.z, basisW.w)) 

      case (1, 2) => Matrix33d(Vector3d(basisX.x, basisX.y, basisX.w), 
                               Vector3d(basisZ.x, basisZ.y, basisZ.w), 
                               Vector3d(basisW.x, basisW.y, basisW.w)) 
      
      case (1, 3) => Matrix33d(Vector3d(basisX.x, basisX.y, basisX.z), 
                               Vector3d(basisZ.x, basisZ.y, basisZ.z), 
                               Vector3d(basisW.x, basisW.y, basisW.z)) 

      case (2, 0) => Matrix33d(Vector3d(basisX.y, basisX.z, basisX.w), 
                               Vector3d(basisY.y, basisY.z, basisY.w), 
                               Vector3d(basisW.y, basisW.z, basisW.w)) 

      case (2, 1) => Matrix33d(Vector3d(basisX.x, basisX.z, basisX.w), 
                               Vector3d(basisY.x, basisY.z, basisY.w),
                               Vector3d(basisW.x, basisW.z, basisW.w)) 

      case (2, 2) => Matrix33d(Vector3d(basisX.x, basisX.y, basisX.w), 
                               Vector3d(basisY.x, basisY.y, basisY.w),
                               Vector3d(basisW.x, basisW.y, basisW.w)) 

      case (2, 3) => Matrix33d(Vector3d(basisX.x, basisX.y, basisX.z), 
                               Vector3d(basisY.x, basisY.y, basisY.z),
                               Vector3d(basisW.x, basisW.y, basisW.z)) 

      case (3, 0) => Matrix33d(Vector3d(basisX.y, basisX.z, basisX.w), 
                               Vector3d(basisY.y, basisY.z, basisY.w), 
                               Vector3d(basisZ.y, basisZ.z, basisZ.w))

      case (3, 1) => Matrix33d(Vector3d(basisX.x, basisX.z, basisX.w), 
                               Vector3d(basisY.x, basisY.z, basisY.w), 
                               Vector3d(basisZ.x, basisZ.z, basisZ.w)) 

      case (3, 2) => Matrix33d(Vector3d(basisX.x, basisX.y, basisX.w), 
                               Vector3d(basisY.x, basisY.y, basisY.w), 
                               Vector3d(basisZ.x, basisZ.y, basisZ.w)) 

      case (3, 3) => Matrix33d(Vector3d(basisX.x, basisX.y, basisX.z), 
                               Vector3d(basisY.x, basisY.y, basisY.z), 
                               Vector3d(basisZ.x, basisZ.y, basisZ.z)) 
      
      case _ => throw new IllegalArgumentException(
        "Invalid column (" + col + ") or row (" + row + ")!")
    }

  /** Find the minor of the given cell (column, row) of this matrix.
    *
    * The minor is the determinant of the 3x3 matrix which remains when the row and column of the
    * given cell are removed from the original 4x4 matrix. */
  def minor(col: Int, row: Int): Double = 
    submatrix(col, row).determinant

  /** Find the matrix of minors of this matrix. */
  def minors: Matrix44d = 
    Matrix44d(Vector4d(minor(0, 0), minor(0, 1), minor(0, 2), minor(0, 3)), 
              Vector4d(minor(1, 0), minor(1, 1), minor(1, 2), minor(1, 3)), 
              Vector4d(minor(2, 0), minor(2, 1), minor(2, 2), minor(2, 3)), 
              Vector4d(minor(3, 0), minor(3, 1), minor(3, 2), minor(3, 3)))

  /** Find the cofactor of the given cell (column, row) of this matrix.
    *
    * The cofactor of a cell is the minor in which the sign of the result is determined by
    * whether the sum of the column and row indices in the original matrix is even (unchanged) 
    * or odd (flipped). */
  def cofactor(col: Int, row: Int): Double = {
    val mm = minor(col, row) 
    if((col+row)%2 == 0) mm else -mm
  }

  /** Find the matrix of cofactors of this matrix */
  def cofactors: Matrix44d = {
    val even = Vector4d(1.0, -1.0, 1.0, -1.0) 
    val odd = even * -1.0
    val mm = minors
    Matrix44d(mm.basisX*even, mm.basisY*odd, mm.basisZ*even, mm.basisW*odd)
  }
	
  /** Find the adjoint of this matrix */
  def adjoint: Matrix44d = 
    cofactors.transpose
	
  /** Find the determinant of this matrix. */
  def determinant: Double = 
    basisX.x*cofactor(0, 0) + basisX.y*cofactor(0, 1) + 
    basisX.z*cofactor(0, 2) + basisX.w*cofactor(0, 3)

  /** Whether the matrix has an inverse (is non-singular). */ 
  def isInvertible = 
    !Scalar.equiv(determinant, 0.0) 

  /** Find the inverse (if possible) of this matrix. */
  def inverse: Option[Matrix44d] = {
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
      case that: Matrix44d => 
        (that canEqual this) &&
        (basisX == that.basisX) && (basisY == that.basisY) && 
        (basisZ == that.basisZ) && (basisW == that.basisW)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean = 
    that.isInstanceOf[Matrix44d]

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 
    53 * (47 * (43 * (41 + basisX.hashCode) + basisY.hashCode) + basisZ.hashCode) + 
    basisW.hashCode
  
  //------------------------------------------------------------------------------------------------

  /** The component-wise comparison of whether the given matrix in within a given
    * epsilon of this matrix. */ 
  def equiv(that: Matrix44d, epsilon: Double): Boolean = 
    forall(that)(Scalar.equiv(_, _, epsilon))
  
  /** The component-wise comparison of whether the given matrix is within a type
    * specific epsilon of this matrix. */ 
  def equiv(that: Matrix44d): Boolean = 
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
      case 3 => basisW
      case _ => throw new IllegalArgumentException("Invalid column (" + col + ")!")
    }
    row match {
      case 0|1|2|3 => b(row)
      case _ => throw new IllegalArgumentException("Invalid row (" + row + ")!")
    }
  }

  
  /** Tests whether the given predicate holds true for all components of this coordinate
    * frame. */ 
  def forall(p: (Double) => Boolean): Boolean = 
    basisX.forall(p) && basisY.forall(p) && basisZ.forall(p) && basisW.forall(p) 

  /** Tests whether the given predicate holds true for all of the corresponding components
    * of this and the given matrix. */ 
  def forall(that: Matrix44d)(p: (Double, Double) => Boolean): Boolean = 
    basisX.forall(that.basisX)(p) && basisY.forall(that.basisY)(p) && 
    basisZ.forall(that.basisZ)(p) && basisW.forall(that.basisW)(p)

  /** Tests whether the given predicate holds true for any components of this coordinate
    * frame. */ 
  def forany(p: (Double) => Boolean): Boolean = 
    basisX.forany(p) && basisY.forany(p) && basisZ.forany(p) && basisW.forany(p) 

  /** Tests whether the given predicate holds true for any of the corresponding components
    * of this and the given matrix. */ 
  def forany(that: Matrix44d)(p: (Double, Double) => Boolean): Boolean = 
    basisX.forany(that.basisX)(p) && basisY.forany(that.basisY)(p) && 
    basisZ.forany(that.basisZ)(p) && basisW.forany(that.basisW)(p)

  /** Applies a function to all components of this matrix.
   *
   * @param f The function that is applied for its side-effect to every component. */
  def foreach(f: (Double) => Unit): Unit = { 
    basisX.foreach(f); basisY.foreach(f); basisZ.foreach(f); basisW.foreach(f)
  }
  
  /** Builds a new matrix by applying a function to each component of this
    * matrix. */
  def map(f: (Double) => Double): Matrix44d = 
    Matrix44d(basisX.map(f), basisY.map(f), basisZ.map(f), basisW.map(f))


  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a nested list of elements (basis vectors XYZW). */ 
  def toList: List[List[Double]] =            
    List(List(basisX.x, basisX.y, basisX.z, basisX.w), 
         List(basisY.x, basisY.y, basisY.z, basisY.w), 
         List(basisZ.x, basisZ.y, basisZ.z, basisZ.w), 
         List(basisW.x, basisW.y, basisW.z, basisW.w))

  /** Convert to a nested array of elements (basis vectors XYZW). */ 
  def toArray: Array[Array[Double]] = 
    Array(Array(basisX.x, basisX.y, basisX.z, basisX.w), 
          Array(basisY.x, basisY.y, basisY.z, basisY.w), 
          Array(basisZ.x, basisZ.y, basisZ.z, basisZ.w), 
          Array(basisW.x, basisW.y, basisW.z, basisW.w))

  /** Add the component values (basis vectors XYZW) starting at the current position to given
    * native array. */ 
  def putNative(buf: DoubleBuffer) {
    buf.put(basisX.x); buf.put(basisX.y); buf.put(basisX.z); buf.put(basisX.w) 
    buf.put(basisY.x); buf.put(basisY.y); buf.put(basisY.z); buf.put(basisY.w) 
    buf.put(basisZ.x); buf.put(basisZ.y); buf.put(basisZ.z); buf.put(basisZ.w) 
    buf.put(basisW.x); buf.put(basisW.y); buf.put(basisW.z); buf.put(basisW.w)
  }

  /** Add the component values (basis vectors XYZW) starting at the current position to given
    * native array of floats. */ 
  def putNativeFloats(buf: FloatBuffer) {
    buf.put(basisX.x.toFloat); buf.put(basisX.y.toFloat); 
    buf.put(basisX.z.toFloat); buf.put(basisX.w.toFloat);

    buf.put(basisY.x.toFloat); buf.put(basisY.y.toFloat);
    buf.put(basisY.z.toFloat); buf.put(basisY.w.toFloat);

    buf.put(basisZ.x.toFloat); buf.put(basisZ.y.toFloat);
    buf.put(basisZ.z.toFloat); buf.put(basisZ.w.toFloat);

    buf.put(basisW.x.toFloat); buf.put(basisW.y.toFloat);
    buf.put(basisW.z.toFloat); buf.put(basisW.w.toFloat);
  }

  /** Convert to a string representation. */
  override def toString() = 
    "Matrix44d(" + basisX + ", " + basisY + ", " + basisZ + ", " + basisW + ")"

}

