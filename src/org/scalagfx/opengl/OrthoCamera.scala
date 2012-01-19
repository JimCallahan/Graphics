package org.scalagfx.opengl

import org.scalagfx.math._

import org.lwjgl.opengl.DisplayMode

//--------------------------------------------------------------------------------------------------
//   O R T H O   C A M E R A 
//--------------------------------------------------------------------------------------------------

/** Factory for OrthoCamera */ 
object OrthoCamera
{
  /** Create an orthogonal projection camera.
    *
    * @param bmin The (left, bottom, near) corner of the clipping box.
    * @param bmax The (right, top, far) corner of the clipping box. */ 
  def apply(bmin: Pos3d, bmax: Pos3d): OrthoCamera = 
    new OrthoCamera(BBox3d(bmin, bmax)) 

  /** Create an orthogonal projection camera.
   *
   * @param left The coordinate of the left clipping plane. 
   * @param right The coordinate of the right clipping plane. 
   * @param bottom The coordinate of the bottom clipping plane. 
   * @param top The coordinate of the top clipping plane. 
   * @param near The coordinate of the near clipping plane. 
   * @param far The coordinate of the far clipping plane. */
  def apply(left: Double, right: Double, 
            bottom: Double, top: Double, 
            near: Double, far: Double): OrthoCamera = 
    new OrthoCamera(BBox3d(Pos3d(left, bottom, near), Pos3d(right, top, far)))

  /** Create an orthogonal projection camera which fits the given display mode so that the smaller
    * dimension will be mapped to [-1,1] range and the larger dimension proportionately.
    *
    * @param mode The OpenGL display mode.
    * @param near The near clipping plane. 
    * @param far The far clipping plane. */
  def apply(mode: DisplayMode, near: Double, far: Double): OrthoCamera = {
    val w = mode.getWidth.toDouble
    val h = mode.getHeight.toDouble
    val range = 
      if(w < h)  
        Vec2d(1.0, h/w)
      else 
        Vec2d(w/h, 1.0)
    val bbox = BBox2d(Pos2d.origin - range, Pos2d.origin + range)

    new OrthoCamera(BBox3d(bbox.bmin.toPos3d.newZ(near), bbox.bmax.toPos3d.newZ(far)))
  }

  /** Create an orthogonal projection camera which fits the given display mode so that the smaller
    * dimension will be mapped to [-1,1] range and the larger dimension proportionately.
    *
    * @param mode The OpenGL display mode. */
  def apply(mode: DisplayMode): OrthoCamera = 
    OrthoCamera(mode, -1.0, 1.0)
}

/** An orthogonal projection camara.
 *
 * @constructor Create a new camera.
 * @param bbox The bounding box in camera space which will be mapped to Normalized Device Coordinate
 * (NDC) space: [-1,1] in all dimensions. */
class OrthoCamera(val bbox: BBox3d) 
{ 
  //------------------------------------------------------------------------------------------------
  //   O P E R A T I O N S
  //------------------------------------------------------------------------------------------------

  /** The projection matrix which transforms from camera space to Normalized Device Coordinate
    * (NDC) space. */ 
  val project = {
    val r = bbox.range
    if(r anyLte Vec3d(0.0)) 
      throw new IllegalArgumentException(
        "The viewing box must have a positive extent in all dimensions!")
      
    val t = (bbox.bmin + bbox.bmax.toVec3d) / r
    Matrix44d(Vector4d(2.0/r.x,     0.0,      0.0, 0.0), 
              Vector4d(    0.0, 2.0/r.y,      0.0, 0.0), 
              Vector4d(    0.0,     0.0, -2.0/r.z, 0.0), 
              Vector4d(   -t.x,    -t.y,     -t.z, 1.0))
  }

  
  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a string representation. */
  override def toString() = 
    "OrthoCamera(" + bbox + ")"
}
