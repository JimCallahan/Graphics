// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.io

import java.security.MessageDigest
import java.io.{File,FileInputStream}

//--------------------------------------------------------------------------------------------------
//  C H E C K S U M   
//--------------------------------------------------------------------------------------------------

/** Factory for Checksum. */
object Checksum {
  //------------------------------------------------------------------------------------------------
  //   D I G E S T                                                                         
  //------------------------------------------------------------------------------------------------

  /** Create a new message digest. */ 
  private def createDigest() = MessageDigest.getInstance("SHA-256")

  /** The name of the message digest algorithm used to create checksums. */
  val algorithm: String = createDigest.getAlgorithm
  
  /** The size of the checksum (in bytes). */ 
  val size: Int = createDigest.getDigestLength


  //------------------------------------------------------------------------------------------------
  //   C R E A T I O N                                                                     
  //------------------------------------------------------------------------------------------------

  /** Create a checksum encoded as a hexadecimal String. */ 
  def apply(str: String): Checksum = {
    if(str.length != size*2) 
      throw new IllegalArgumentException(
        "The number of hexidecimal characters in a Checksum must be (" + (size*2) + ")!")
    new Checksum(fromHex(str))
  }
  
  /** Create a checksum of the given data. */ 
  def apply(data: Array[Byte]): Checksum = {
    val md = createDigest
    md.update(data)
    new Checksum(md.digest)
  }

  /** Create a checksum of the data contained in the given file. */ 
  def apply(path: Path): Checksum = 
    Checksum(path.toFile)

  /** Create a checksum of the data contained in the given file. */ 
  def apply(file: File): Checksum = {
    val md = createDigest
    val in = new FileInputStream(file) 
    try {
      val buf = new Array[Byte](4096)
      def f() {
        in.read(buf) match {
          case -1 => () 
            case n  => md.update(buf, 0, n); f() 
        }
      }
      f() 
    }
    finally {
      in.close
    }
    new Checksum(md.digest)
  }


  //------------------------------------------------------------------------------------------------
  //   H E L P E R S 
  //------------------------------------------------------------------------------------------------

  /** The hexadecimal digits. */ 
  private val hexDigits = 
    Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')

  /** Convert the given bytes to a hexadecimal string. */ 
  private def toHex(bytes: Array[Byte]): String = {
    val len = bytes.length
    val cs = new Array[Char](len*2)
    for(i <- 0 until len) {
      val v = (bytes(i) + 256) % 256
      val hi = v >> 4
      val low = v & 0x0f
      cs(i*2)   = hexDigits(hi)
      cs(i*2+1) = hexDigits(low)
    }
    new String(cs)
  }
  
  /** Convert the given hexadecimal string to bytes. */ 
  private def fromHex(hex: String): Array[Byte] = {
    val cs = hex.toCharArray
    val len = cs.length / 2
    val bytes = new Array[Byte](len)
    for(i <- 0 until len) {
      val hi = Character.digit(cs(i*2), 16)
      val lo = Character.digit(cs(i*2+1), 16)
      val v = (hi << 4) | lo
      bytes(i) = (if(v > 127) v - 256 else v).toByte
    }
    bytes
  }
}

/** A checksum of some data.
  *
  * @constructor Create a checksum.
  * @param bytes The checksum value. */ 
class Checksum(val bytes: Array[Byte]) {
  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Compares this checksum to the specified value for equality. */
  override def equals(that: Any): Boolean = 
    that match {
      case that: Checksum => 
        (that canEqual this) && (toString == (that.toString))
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean = 
    that.isInstanceOf[Checksum]

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 
    bytes.hashCode


  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a hexadecimal string representation. */ 
  override def toString(): String = 
    Checksum.toHex(bytes)
}
