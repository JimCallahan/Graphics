// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.opengl

import org.scalagfx.math._

import org.lwjgl.opengl.DisplayMode

//--------------------------------------------------------------------------------------------------
//   P E R S P   C A M E R A 
//--------------------------------------------------------------------------------------------------

/** Factory for PerspCamera */ 
object PerspCamera
{
  /** Create a perspective projection camera which fits the given display mode.
   *
   * @param mode The OpenGL display mode.
   * @param fov The horizontal field of view (in degrees).
   * @param near The near clipping plane. 
   * @param far The far clipping plane. */
  def apply(mode: DisplayMode, fov: Double, near: Double, far: Double) = {
    val w = mode.getWidth.toDouble
    val h = mode.getHeight.toDouble
    new PerspCamera(fov, w/h, near, far)
  }
}

/** A perspective projection camera.
 *
 * @constructor Create a new camera.
 * @param fov The horizontal field of view (in degrees).
 * @param aspect The aspect ratio (width/height) of the frustum.
 * @param near The near clipping plane. 
 * @param far The far clipping plane. */
class PerspCamera(val fov: Double, val aspect: Double, val near: Double, val far: Double)
{ 
  //------------------------------------------------------------------------------------------------
  //   O P E R A T I O N S
  //------------------------------------------------------------------------------------------------

  /** The projection matrix which transforms from camera space to Normalized Device Coordinate
    * (NDC) space. */ 
  val project = {
    if(Scalar.equiv(aspect, 0.0) || (aspect < 0.0)) 
      throw new IllegalArgumentException("The aspect ratio must be positive!") 
    if(Scalar.equiv(fov, 0.0) || (fov < 0.0) || (fov >= 180.0)) 
      throw new IllegalArgumentException(
        "The field of view must be positive and less than 180 degrees!") 

    import scala.math.{Pi,tan}
    val w = tan((fov*Pi)/360.0)
    val r = near - far

    if(Scalar.equiv(r, 0.0)) 
      throw new IllegalArgumentException("The near and far clipping planes cannot be coincident!")
      
    Matrix44d(Vector4d(1.0/(aspect*w),        0.0,              0.0,  0.0), 
              Vector4d(           0.0, 1.0/aspect,              0.0,  0.0), 
              Vector4d(           0.0,        0.0,     (far+near)/r, -1.0), 
              Vector4d(           0.0,        0.0, (2.0*far*near)/r,  0.0))              
  }

  
  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a string representation. */
  override def toString() = 
    "PerspCamera(" + fov + ", " + aspect + ", " + near  + ", " + far  + ")"
}
