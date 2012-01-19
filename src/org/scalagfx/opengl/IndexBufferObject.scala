package org.scalagfx.opengl

import org.lwjgl._
import org.lwjgl.opengl._
import GL11._
import GL15._

import java.nio._

//--------------------------------------------------------------------------------------------------
//   I N D E X   B U F F E R   O B J E C T                                                
//--------------------------------------------------------------------------------------------------

/** Index Buffer Object. 
  *
  * @constructor Create a new Index Buffer Object.
  * @param dataType The OpenGL storage type of the buffer. 
  * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
  * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
  * GL_DYNAMIC_READ or GL_DYNAMIC_COPY. */
abstract class IndexBufferObject(val dataType: Int, val usage: Int) 
  extends BufferObject(GL_ELEMENT_ARRAY_BUFFER, "IBO") 


//--------------------------------------------------------------------------------------------------

/** Factory for ByteIBO. */ 
object ByteIBO 
{
  /** Create a IBO to hold Byte index data.
    *
    * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
    * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
    * GL_DYNAMIC_READ or GL_DYNAMIC_COPY. */
  def apply(usage: Int = GL_STATIC_DRAW): ByteIBO = 
    new ByteIBO(usage)
}
    
/** Vertex Buffer Object containing Byte attribute data. */ 
class ByteIBO(usage: Int) 
  extends IndexBufferObject(GL_UNSIGNED_BYTE, usage) 
{

  import ObjState._

  /** Initial copy of client data to OpenGL server-side buffer storage. 
    *
    * @param data The client data to be coped. */
  def load(data: ByteBuffer) {
    state match {
      case Reserved => {
        bind
        glBufferData(target, data, usage)
        unbind
        state = Loaded
      }
      case _ => throw new OpenGLException(
        "Unable to load client data because the buffer object was in a " + state + " state!")
    }
  }

  /** Reload some or all client data to OpenGL server-side buffer storage. 
    *
    * @param data The client data to be coped. 
    * @param offset The offset into server-side buffer where replacement will begin. */ 
  def reload(data: ByteBuffer, offset: Long) {
    state match {
      case Loaded => {
        bind
        glBufferSubData(target, offset, data)
        unbind
      }
      case _ => throw new OpenGLException(
        "Unable to reload client data because the buffer object was in a " + state + " state!")
    }
  }
}


//--------------------------------------------------------------------------------------------------

/** Factory for ShortIBO. */ 
object ShortIBO 
{
  /** Create a IBO to hold Short index data.
    *
    * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
    * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
    * GL_DYNAMIC_READ or GL_DYNAMIC_COPY. */
  def apply(usage: Int = GL_STATIC_DRAW): ShortIBO = 
    new ShortIBO(usage)
}
    
/** Vertex Buffer Object containing Short attribute data. */ 
class ShortIBO(usage: Int) 
  extends IndexBufferObject(GL_UNSIGNED_SHORT, usage) 
{
  import ObjState._

  /** Initial copy of client data to OpenGL server-side buffer storage. 
    *
    * @param data The client data to be coped. */
  def load(data: ShortBuffer) {
    state match {
      case Reserved => {
        bind
        glBufferData(target, data, usage)
        unbind
        state = Loaded
      }
      case _ => throw new OpenGLException(
        "Unable to load client data because the buffer object was in a " + state + " state!")
    }
  }

  /** Reload some or all client data to OpenGL server-side buffer storage. 
    *
    * @param data The client data to be coped. 
    * @param offset The offset into server-side buffer where replacement will begin. */ 
  def reload(data: ShortBuffer, offset: Long) {
    state match {
      case Loaded => {
        bind
        glBufferSubData(target, offset, data)
        unbind
      }
      case _ => throw new OpenGLException(
        "Unable to reload client data because the buffer object was in a " + state + " state!")
    }
  }
}


//--------------------------------------------------------------------------------------------------

/** Factory for IntIBO. */ 
object IntIBO 
{
  /** Create a IBO to hold Int index data.
    *
    * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
    * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
    * GL_DYNAMIC_READ or GL_DYNAMIC_COPY. */
  def apply(usage: Int = GL_STATIC_DRAW): IntIBO = 
    new IntIBO(usage)
}
    
/** Vertex Buffer Object containing Int attribute data. */ 
class IntIBO(usage: Int) 
  extends IndexBufferObject(GL_UNSIGNED_INT, usage) 
{
  import ObjState._

  /** Initial copy of client data to OpenGL server-side buffer storage. 
    *
    * @param data The client data to be coped. */
  def load(data: IntBuffer) {
    state match {
      case Reserved => {
        bind
        glBufferData(target, data, usage)
        unbind
        state = Loaded
      }
      case _ => throw new OpenGLException(
        "Unable to load client data because the buffer object was in a " + state + " state!")
    }
  }

  /** Reload some or all client data to OpenGL server-side buffer storage. 
    *
    * @param data The client data to be coped. 
    * @param offset The offset into server-side buffer where replacement will begin. */ 
  def reload(data: IntBuffer, offset: Long) {
    state match {
      case Loaded => {
        bind
        glBufferSubData(target, offset, data)
        unbind
      }
      case _ => throw new OpenGLException(
        "Unable to reload client data because the buffer object was in a " + state + " state!")
    }
  }
}
