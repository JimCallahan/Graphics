// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.opengl

import org.lwjgl._
import org.lwjgl.opengl._
import GL15._

import java.nio._

//--------------------------------------------------------------------------------------------------
//   B U F F E R   O B J E C T                                                
//--------------------------------------------------------------------------------------------------

/** The abstract base class of all OpenGL server-side data buffer objects. 
  *
  * @constructor Create a new Buffer Object.
  * @param target The target (OpenGL enum) to which kind of buffer object is bound:
  * GL_ARRAY_BUFFER, GL_COPY_READ_BUFFER, GL_COPY_WRITE_BUFFER, GL_ELEMENT_ARRAY_BUFFER,
  * GL_PIXEL_PACK_BUFFER, GL_PIXEL_UNPACK_BUFFER, GL_TEXTURE_BUFFER, GL_TRANSFORM_FEEDBACK_BUFFER
  * or GL_UNIFORM_BUFFER. 
  * @param The name for this type of OpenGL object. */ 
abstract class BufferObject(val target: Int, objName: String) 
  extends BaseObject(objName) 
{
  import ObjState._

  /** The unique OpenGL handle for the buffer object. */ 
  protected val objectID = {
    val id = glGenBuffers
    Util.checkGLError
    state = Reserved
    id
  }

  /** Perform the OpenGL call which binds the specific type of object. */
  protected def bindGL() {
    glBindBuffer(target, objectID)
  }

  /** Perform the OpenGL call which unbinds the specific type of object. */
  protected def unbindGL() {
    glBindBuffer(target, 0)
  }

  /** Perform the OpenGL call which releases all server-side resources. */ 
  protected def releaseGL() {
    glDeleteBuffers(objectID)
  }
}
