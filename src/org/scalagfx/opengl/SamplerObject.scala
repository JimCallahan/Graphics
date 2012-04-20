// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.opengl

import org.scalagfx.math.Index2i

import org.lwjgl._
import org.lwjgl.opengl._
import GL11._
import GL12._
import GL13._
import GL33._

import java.nio._

//--------------------------------------------------------------------------------------------------
//   S A M P L E R
//--------------------------------------------------------------------------------------------------

/** An OpenGL texture sampler object. 
  *
  * @constructor Create a new Sampler Object.
  * @param textureUnit The ID of the hardware texture unit where this sampler will be bound. */ 
class SamplerObject(val textureUnit: Int) 
  extends BaseObject("Sampler")
{
  import ObjState._

  /** The unique OpenGL handle for the sampler object. */ 
  protected val objectID = {
    val id = glGenSamplers
    Util.checkGLError
    state = Reserved
    id
  }

  /** Perform the OpenGL call which binds the specific type of object. */
  protected def bindGL() {
    glActiveTexture(GL_TEXTURE0 + textureUnit)
    glBindSampler(textureUnit, objectID)
  }

  /** Perform the OpenGL call which unbinds the specific type of object. */
  protected def unbindGL() {
    glActiveTexture(GL_TEXTURE0 + textureUnit)
    glBindSampler(textureUnit, 0)
  }

  /** Perform the OpenGL call which releases all server-side resources. */ 
  protected def releaseGL() {
    glDeleteSamplers(objectID)
  }
}


//--------------------------------------------------------------------------------------------------

/** Factory for MipmapSampler. */
object MipmapSampler
{
  /** Create an OpenGL sampler of mipmapped textures.
   * 
  * @constructor Create a new Sampler Object.
  * @param textureUnit The ID of the hardware texture unit where this sampler will be bound.
  * @param wrapS The wrap parameter (OpenGL enum) for the S-coordinate of the texture:
  * GL_CLAMP_TO_EDGE, GL_MIRRORED_REPEAT or GL_REPEAT.
  * @param wrapT The wrap parameter (OpenGL enum) for the T-coordinate of the texture:
  * GL_CLAMP_TO_EDGE, GL_MIRRORED_REPEAT or GL_REPEAT. */
  def apply(textureUnit: Int, wrapS: Int, wrapT: Int): MipmapSampler = 
    new MipmapSampler(textureUnit, wrapS, wrapT)
}

/** An OpenGL sampler of mipmapped textures. */
class MipmapSampler(textureUnit: Int, val wrapS: Int, val wrapT: Int) 
  extends SamplerObject(textureUnit) 
{
  // Initialize sampler parameters.
  { 
    glSamplerParameteri(objectID, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glSamplerParameteri(objectID, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR) 
    glSamplerParameteri(objectID, GL_TEXTURE_WRAP_S, wrapS)
    glSamplerParameteri(objectID, GL_TEXTURE_WRAP_T, wrapT) 
    Util.checkGLError
  }
}

//--------------------------------------------------------------------------------------------------

/** Factory for ClampedMipmapSampler. */
object ClampedMipmapSampler
{
  /** Create an OpenGL sampler of mipmapped textures which clamps texture coordinates.
   * 
  * @constructor Create a new Sampler Object.
  * @param textureUnit The ID of the hardware texture unit where this sampler will be bound. */
  def apply(textureUnit: Int): ClampedMipmapSampler = 
    new ClampedMipmapSampler(textureUnit)
}

/** An OpenGL sampler of mipmapped textures which clamps texture coordinates.  */
class ClampedMipmapSampler(textureUnit: Int) 
  extends MipmapSampler(textureUnit, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE)
