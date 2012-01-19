package org.scalagfx.math

import scala.math.{abs,sin,cos}
import java.nio._

//--------------------------------------------------------------------------------------------------
//   M A T R I X   L I K E
//--------------------------------------------------------------------------------------------------

/** A set of operations common to all Matrix-like classes. */ 
trait MatrixLike
{
  //------------------------------------------------------------------------------------------------
  //   U N A R Y   O P S                                                                    
  //------------------------------------------------------------------------------------------------

  /** The number of dimensions. */
  val dimens: Int

  
  //------------------------------------------------------------------------------------------------
  //   U T I L I T Y                                                                        
  //------------------------------------------------------------------------------------------------

  /** Lookup a the value of a given cell (column, row) of this matrix. */ 
  def apply(col: Int, row: Int): Double
  

  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a native array (basis vectors in order). */
  def toNative: DoubleBuffer = {
    val size = dimens*dimens
    val buf = ByteBuffer.allocateDirect(size << 3).order(ByteOrder.nativeOrder).asDoubleBuffer
    putNative(buf)
    buf.rewind
    buf
  }

  /** Add the component values (basis vectors in order) starting at the current position to given
    * native array. */ 
  def putNative(buf: DoubleBuffer)

  /** Add the component values (basis vectors in order) starting at the current position to given
    * native array. */ 
  def -> (buf: DoubleBuffer) {
    putNative(buf)
  }

  /** Convert to a native array (basis vectors in order) of floats. */
  def toNativeFloats: FloatBuffer = {
    val size = dimens*dimens
    val buf = ByteBuffer.allocateDirect(size << 2).order(ByteOrder.nativeOrder).asFloatBuffer
    putNativeFloats(buf)
    buf.rewind
    buf
  }

  /** Add the component values (basis vectors in order) starting at the current position to given
    * native array of floats. */ 
  def putNativeFloats(buf: FloatBuffer)

  /** Add the component values (basis vectors in order) starting at the current position to given
    * native array of floats. */ 
  def -> (buf: FloatBuffer) {
    putNativeFloats(buf)
  }

  /** Pretty print the matrix. */ 
  def printMatrix() = {
    val (widths, cols) = 
      (for(i <- 0 until dimens) yield { 
        val col = for(j <- 0 until dimens) yield { "%.2f".format(this(i, j)) } 
        ((0 /: col)((b, a) => scala.math.max(b, a.size)), col) 
      }).unzip

    def pad(size: Int): String = size match { 
      case 0 => ""
      case n if n > 0 => " " + pad(n-1)
      case _ => "" 
    }
    
    for(j <- 0 until dimens) {
      print("|") 
      for(i <- 0 until dimens) {
        val s = cols(i)(j)
        print(pad(widths(i) - s.size + 1) + s)
      }
      println(" |")
    }
  }

}

