// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.opengl

import org.scalagfx.math.Index2i

import org.lwjgl._
import org.lwjgl.opengl._
import GL11._
import GL12._
import GL13._
import GL15._
import GL20._
import GL30._

import java.nio._

//--------------------------------------------------------------------------------------------------
//   T E X T U R E
//--------------------------------------------------------------------------------------------------


/** The abstract base class of all OpenGL server-side data buffer objects. 
  *
  * @constructor Create a new Texture Object.
  * @param target The target (OpenGL enum) to which kind of texture object is bound:
  * GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, or GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY,
  * GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_2D_MULTISAMPLE or
  * GL_TEXTURE_2D_MULTISAMPLE_ARRAY. 
  * @param textureUnit The ID of the hardware texture unit where this texture will be bound. */ 
abstract class TextureObject(val target: Int, val textureUnit: Int) 
  extends BaseObject("Texture") 
{
  import ObjState._

  /** The unique OpenGL handle for the texture object. */ 
  protected val objectID = {
    val id = glGenTextures
    Util.checkGLError
    state = Reserved
    id
  }

  /** Perform the OpenGL call which binds the specific type of object. */
  protected def bindGL() {
    glActiveTexture(GL_TEXTURE0 + textureUnit)
    glBindTexture(target, objectID)
  }

  /** Perform the OpenGL call which unbinds the specific type of object. */
  protected def unbindGL() {
    glActiveTexture(GL_TEXTURE0 + textureUnit)
    glBindTexture(target, 0)
  }

  /** Perform the OpenGL call which releases all server-side resources. */ 
  protected def releaseGL() {
    glDeleteTextures(objectID)
  }
}


//--------------------------------------------------------------------------------------------------

/** Factory for Texture1D. */ 
object Texture1D
{
  /** Create a 1-dimensional simple texture.
    *
    * @param size The base mipmap level texture image size (should be power of 2).
    * @param iformat The internal data format (OpenGL enum): GL_COMPRESSED_RED, GL_COMPRESSED_RG,
    * GL_COMPRESSED_RGB, GL_COMPRESSED_RGBA, GL_COMPRESSED_SRGB, GL_COMPRESSED_SRGB_ALPHA,
    * GL_DEPTH_COMPONENT, GL_DEPTH_COMPONENT16, GL_DEPTH_COMPONENT24, GL_DEPTH_COMPONENT32,
    * GL_R3_G3_B2, GL_RED, GL_RG, GL_RGB, GL_RGB4, GL_RGB5, GL_RGB8, GL_RGB10, GL_RGB12, GL_RGB16,
    * GL_RGBA, GL_RGBA2, GL_RGBA4, GL_RGB5_A1, GL_RGBA8, GL_RGB10_A2, GL_RGBA12, GL_RGBA16, GL_SRGB,
    * GL_SRGB8, GL_SRGB_ALPHA or GL_SRGB8_ALPHA8.
    * @param textureUnit The ID of the hardware texture unit where this texture will be bound. */
  def apply(size: Int, iformat: Int, textureUnit: Int): Texture1D =
    new Texture1D(size, iformat, textureUnit)
}
/** A 1-dimensional simple texture. */ 
class Texture1D(val size: Int, val iformat: Int, textureUnit: Int) 
  extends TextureObject(GL_TEXTURE_1D, textureUnit)
{
  /** Load texture data.
   *
   * @param format The format (OpenGL enum) of the pixel data: GL_RED, GL_RG, GL_RGB, GL_BGR,
   * GL_RGBA or GL_BGRA.
   * @param data The pixel data to load. */ 
  def load(format: Int, data: ByteBuffer) {
    bind
    glTexImage1D(target, 0, iformat, size, 0, format, GL_UNSIGNED_BYTE, data)
    unbind
  }

  // ..

}


//--------------------------------------------------------------------------------------------------

/** Factory for Texture2D. */ 
object Texture2D
{
  /** Create a 2-dimensional mipmapped texture.
   *
   * @param size The base mipmap level texture image size (should be power of 2).
   * @param iformat The internal data format (OpenGL enum): GL_COMPRESSED_RED, GL_COMPRESSED_RG,
   * GL_COMPRESSED_RGB, GL_COMPRESSED_RGBA, GL_COMPRESSED_SRGB, GL_COMPRESSED_SRGB_ALPHA,
   * GL_DEPTH_COMPONENT, GL_DEPTH_COMPONENT16, GL_DEPTH_COMPONENT24, GL_DEPTH_COMPONENT32,
   * GL_R3_G3_B2, GL_RED, GL_RG, GL_RGB, GL_RGB4, GL_RGB5, GL_RGB8, GL_RGB10, GL_RGB12, GL_RGB16,
   * GL_RGBA, GL_RGBA2, GL_RGBA4, GL_RGB5_A1, GL_RGBA8, GL_RGB10_A2, GL_RGBA12, GL_RGBA16, GL_SRGB,
   * GL_SRGB8, GL_SRGB_ALPHA or GL_SRGB8_ALPHA8.
   * @param textureUnit The ID of the hardware texture unit where this texture will be bound. */ 
  def apply(size: Index2i, iformat: Int, textureUnit: Int): Texture2D =
    new Texture2D(size, iformat, textureUnit) 
}

/** A 2-dimensional mipmapped texture. */ 
class Texture2D(val size: Index2i, val iformat: Int, textureUnit: Int) 
  extends TextureObject(GL_TEXTURE_2D, textureUnit)
{
  /** The number of mipmap levels. */ 
  val levels = {
    import scala.math.{log,min}
    val power = size.toVec2d.map(c => log(c.toDouble)/log(2.0))
    if(power.forany(c => c.floor != c)) 
      throw new IllegalArgumentException(
        "The texture image resolution must be a power of (2)!")    
    (power.reduce(min)).toInt
  }

  /** Load texture data and generate mipmaps.
   *
   * @param format The format (OpenGL enum) of the pixel data: GL_RED, GL_RG, GL_RGB, GL_BGR,
   * GL_RGBA or GL_BGRA.
   * @param data The pixel data to load. */ 
  def load(format: Int, data: ByteBuffer) {
    bind
    glTexParameteri(target, GL_TEXTURE_BASE_LEVEL, 0)
    glTexParameteri(target, GL_TEXTURE_MAX_LEVEL, levels)
    glTexImage2D(target, 0, iformat, size.x, size.y, 0, format, GL_UNSIGNED_BYTE, data)
    glGenerateMipmap(target)
    unbind
  }

  // add loaders for other datatypes... 

  // Add a way to associate this texture with a specific texture unit from a shader
  // bind; glActiveTexture; ubind ... 

}


//--------------------------------------------------------------------------------------------------

/** Factory for TestTexture2D. */ 
object TestTexture2D
{
  /** Create a procedurally generated 2-dimensional mipmapped texture suitable for testing. 
    *
    * @param size The size of the square texgture in both dimensions.
    * @param textureUnit The ID of the hardware texture unit where this texture will be bound. */ 
  def apply(size: Int, textureUnit: Int): Texture2D =
    new TestTexture2D(size, textureUnit) 
}

/** A procedurally generated 2-dimensional mipmapped texture suitable for testing. */ 
class TestTexture2D(val sz: Int, textureUnit: Int)
  extends Texture2D(Index2i(sz), GL_RGBA, textureUnit)
{
  val buf = BufferUtils.createByteBuffer(sz*sz*4)

  val eigth = sz / 8  
  for(y <- 0 until sz; x <- 0 until sz) {
    val v = Index2i(x, y).toVec2d / size.toVec2d
    val rg = (v * 255.0).toIndex2i
    if(((x/eigth) + (y/eigth))%2 == 0) {
      buf.put(rg.x.toByte)
      buf.put(rg.y.toByte)
    }
    else {
      buf.put(128.toByte)
      buf.put(128.toByte)
    }

    buf.put(((v.x * v.y) * 255.0).toByte)
    buf.put(255.toByte)
  }
  buf.rewind
  
  load(GL_RGBA, buf)
}
