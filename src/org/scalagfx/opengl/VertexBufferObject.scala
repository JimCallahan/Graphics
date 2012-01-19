package org.scalagfx.opengl

import org.lwjgl._
import org.lwjgl.opengl._
import GL11._
import GL15._

import java.nio._

//--------------------------------------------------------------------------------------------------
//   V E R T E X   B U F F E R   O B J E C T                                                
//--------------------------------------------------------------------------------------------------

/** Abstract base class for all Vertex Buffer Objects.
  *
  * @constructor Create a new Vertex Buffer Object.
  * @param dataType The OpenGL storage type of the buffer. 
  * @param attrSize The number of attribute values per vertex: 1, 2, 3 or 4.
  * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
  * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
  * GL_DYNAMIC_READ or GL_DYNAMIC_COPY. */
abstract class VertexBufferObject(val dataType: Int, val attrSize: Int, val usage: Int) 
  extends BufferObject(GL_ARRAY_BUFFER, "VBO") 
{    
  if((attrSize < 1) || (attrSize > 4)) 
    throw new OpenGLException("VBO attribute size (" + attrSize + ") was outside 1-4 range!")
}


//--------------------------------------------------------------------------------------------------

/** Factory for ByteVBO. */ 
object ByteVBO 
{
  /** Create a VBO to hold Byte data.
    *
    * @param attrSize The number of attribute values per vertex: 1, 2, 3 or 4
    * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
    * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
    * GL_DYNAMIC_READ or GL_DYNAMIC_COPY. */
  def apply(attrSize: Int = 4, usage: Int = GL_STATIC_DRAW): ByteVBO = 
    new ByteVBO(attrSize, usage)
}
    
/** Vertex Buffer Object containing Byte attribute data. */ 
class ByteVBO(attrSize: Int, usage: Int) 
  extends VertexBufferObject(GL_UNSIGNED_BYTE, attrSize, usage) 
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
  def reload(data: ByteBuffer, offset: Long = 0L) {
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

/** Factory for ShortVBO. */ 
object ShortVBO 
{
  /** Create a VBO to hold Short data.
    *
    * @param attrSize The number of attribute values per vertex: 1, 2, 3 or 4
    * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
    * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
    * GL_DYNAMIC_READ or GL_DYNAMIC_COPY. */
  def apply(attrSize: Int = 4, usage: Int = GL_STATIC_DRAW): ShortVBO = 
    new ShortVBO(attrSize, usage)
}
    
/** Vertex Buffer Object containing Short attribute data. */ 
class ShortVBO(attrSize: Int, usage: Int) 
  extends VertexBufferObject(GL_UNSIGNED_SHORT, attrSize, usage) 
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
  def reload(data: ShortBuffer, offset: Long = 0L) {
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

/** Factory for IntVBO. */ 
object IntVBO 
{
  /** Create a VBO to hold Int data.
    *
    * @param attrSize The number of attribute values per vertex: 1, 2, 3 or 4
    * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
    * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
    * GL_DYNAMIC_READ or GL_DYNAMIC_COPY. */
  def apply(attrSize: Int = 4, usage: Int = GL_STATIC_DRAW): IntVBO = 
    new IntVBO(attrSize, usage)
}
    
/** Vertex Buffer Object containing Int attribute data. */ 
class IntVBO(attrSize: Int, usage: Int) 
  extends VertexBufferObject(GL_UNSIGNED_INT, attrSize, usage) 
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
  def reload(data: IntBuffer, offset: Long = 0L) {
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

/** Factory for FloatVBO. */ 
object FloatVBO 
{
  /** Create a VBO to hold Float data.
    *
    * @param attrSize The number of attribute values per vertex: 1, 2, 3 or 4
    * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
    * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
    * GL_DYNAMIC_READ or GL_DYNAMIC_COPY. */
  def apply(attrSize: Int = 4, usage: Int = GL_STATIC_DRAW): FloatVBO = 
    new FloatVBO(attrSize, usage)
}
    
/** Vertex Buffer Object containing Float attribute data. */ 
class FloatVBO(attrSize: Int, usage: Int) 
  extends VertexBufferObject(GL_FLOAT, attrSize, usage) 
{
  import ObjState._

  /** Initial copy of client data to OpenGL server-side buffer storage. 
    *
    * @param data The client data to be coped. */
  def load(data: FloatBuffer) {
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
  def reload(data: FloatBuffer, offset: Long = 0L) {
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

/** Factory for DoubleVBO. */ 
object DoubleVBO 
{
  /** Create a VBO to hold Double data.
    *
    * @param attrSize The number of attribute values per vertex: 1, 2, 3 or 4
    * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
    * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
    * GL_DYNAMIC_READ or GL_DYNAMIC_COPY. */
  def apply(attrSize: Int = 4, usage: Int = GL_STATIC_DRAW): DoubleVBO = 
    new DoubleVBO(attrSize, usage)
}
    
/** Vertex Buffer Object containing Double attribute data. */ 
class DoubleVBO(attrSize: Int, usage: Int) 
  extends VertexBufferObject(GL_DOUBLE, attrSize, usage) 
{
  import ObjState._

  /** Initial copy of client data to OpenGL server-side buffer storage. 
    *
    * @param data The client data to be coped. */
  def load(data: DoubleBuffer) {
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
  def reload(data: DoubleBuffer, offset: Long = 0L) {
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
