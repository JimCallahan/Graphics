// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.opengl

import org.lwjgl._
import org.lwjgl.opengl._
import GL11._
import GL12._
import GL15._
import GL20._
import GL30._

//--------------------------------------------------------------------------------------------------
//   V E R T E X   A R R A Y   O B J E C T                                                
//--------------------------------------------------------------------------------------------------

/** Factory for VertexArrayObject */ 
object VertexArrayObject 
{
  /** Create a new VAO. */
  def apply(vbos: List[VertexBufferObject], ibo: IndexBufferObject) = 
    new VertexArrayObject(vbos, ibo)
}

/** Vertex Array Object. */
class VertexArrayObject(val vbos: List[VertexBufferObject], val ibo: IndexBufferObject)
  extends BaseObject("VAO") 
{
  /** The unique OpenGL handle for the VAO. */ 
  protected val objectID = {
    val id = glGenVertexArrays 
    Util.checkGLError
    state = ObjState.Reserved 
    id
  }

  // Initialize the VAO.
  {
    bind
    for((vbo, idx) <- vbos.zip(0 until vbos.length)) {
      vbo.bind
      glEnableVertexAttribArray(idx)
      glVertexAttribPointer(idx, vbo.attrSize, vbo.dataType, false, 0, 0)
    }
    ibo.bind

    Util.checkGLError

    unbind
    for(idx <- 0 until vbos.length) 
      glDisableVertexAttribArray(idx)
    if(!vbos.isEmpty)
      vbos.head.unbind
    ibo.unbind

    state = ObjState.Loaded  
  }

  /** Bind the VAO to the current context. */ 
  protected def bindGL() { 
    glBindVertexArray(objectID) 
  }

  /** Unbind the VAO from the current context. */     
  protected def unbindGL() {
    glBindVertexArray(0)
  }

  /** Release the OpenGL resources for the VAO. */
  protected def releaseGL() {
    glDeleteVertexArrays(objectID)
  }
}


