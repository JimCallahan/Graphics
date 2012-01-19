package org.scalagfx.core

import org.scalagfx.math._
import org.scalagfx.opengl._

import org.lwjgl.input._
import org.lwjgl.opengl._

//--------------------------------------------------------------------------------------------------
//   Z O O M   C O N T R O L L E R
//--------------------------------------------------------------------------------------------------

/** Controlls the position and zoom of a camera using mouse input. */ 
object ZoomController
{
  /** Previous mouse screen position. */ 
  private var prevPos: Vec2d = Vec2d(0.0)

  /** Whether the button 0 was pressed. */ 
  private var isPressed: Boolean = false

  /** Get the motion of the mouse while button 0 is pressed, if any. */
  private def drag(): Option[Vec2d] = {
    val pos = Index2i(Mouse.getX, Mouse.getY).toVec2d
    if(Mouse.isButtonDown(0)) {
      val delta = if(!isPressed) Vec2d(0.0) else (pos - prevPos)
      isPressed = true        
      prevPos = pos
      Some(delta) 
    }
    else {
      isPressed = false
      None
    }
  }

  /** Get the motion of the mouse wheel, if any. */
  private def wheel(): Option[Double] = {
    val wheel = Mouse.getDWheel
    if(wheel == 0) None else Some(wheel.toDouble)
  }

  /** Scaling factor of wheel movement to zoom delta. */ 
  private val zoomFactor = 10e-4

  /** The range of allowable zoom levels. */ 
  private val zoomRange = Interval(-15.0, 15.0)

  /** Create a new camera with modified position and zoom controlled by mouse input. */ 
  def control(cam: ZoomCamera): ZoomCamera = {
    val newZoom = 
      wheel match {
        case Some(delta) => 
          Scalar.clamp(cam.zoom + delta*zoomFactor, zoomRange.lower, zoomRange.upper) 
        case _ => cam.zoom
      }
    
    val newPos = 
      drag match {
        case Some(delta) => cam.pos + (delta*cam.factor)/scala.math.pow(2.0, newZoom)
        case _ => cam.pos
      }
    
    cam.update(newZoom, newPos)
  }
}

