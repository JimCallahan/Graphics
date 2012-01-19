package org.scalagfx.opengl

import org.lwjgl._
import org.lwjgl.opengl._
import GL15._

import java.nio._

//--------------------------------------------------------------------------------------------------
//   B A S E   O B J E C T                                                
//--------------------------------------------------------------------------------------------------

/** The abstract base class of all OpenGL server-side objects. 
  *
  * @constructor Create a new OpenGL Object.
  * @param The name for this type of OpenGL object. */ 
abstract class BaseObject(objName: String) 
{
  object ObjState extends Enumeration {
    /** Initial state of the object before any OpenGL calls are made. */ 
    val Initial = Value

    /** The object handle has been reserved, but no data has been allocated or copied. */ 
    val Reserved = Value

    /** The server-side data has been allocated and at least an initial set of client values
      * copied so that the object is ready for use. */ 
    val Loaded = Value

    /** The server-side storage has been released (deleted) leaving this instance useless. */ 
    val Released = Value
  }
  import ObjState._

  /** State of the OpenGL server-side object. */ 
  protected var state: ObjState.Value = Initial

  /** Whether the object is ready for use. */ 
  def isValid(): Boolean = 
    state match {
      case Loaded => true
      case _ => false
    }

  /** Perform the OpenGL call which binds the specific type of object. */
  protected def bindGL()  

  /** Bind the object to the current context. */ 
  def bind() { 
    state match {
      case Reserved | Loaded => {
        bindGL() 
      }
      case _ => throw new OpenGLException(
        "Unable to bind " + objName + " because it was in a " + state + " state!")
    }
  }

  /** Perform the OpenGL call which unbinds the specific type of object. */
  protected def unbindGL()  

  /** Unbind the object from the current context. */     
  def unbind() {
    state match {
      case Reserved | Loaded => {
        unbindGL() 
      }
      case _ => throw new OpenGLException(
        "Unable to unbind " + objName + " because it was in a " + state + " state!")
    }
  }

  /** Perform the OpenGL call which releases all server-side resources. */ 
  protected def releaseGL()  

  /** Release all OpenGL server-side resources. */
  def release() {
    state match {
      case Loaded => {
        releaseGL() 
        state = Released 
      }
      case _ => throw new OpenGLException(
        "Unable to release " + objName + " because it was in a " + state + " state!")
    }
  }
}
