package org.scalagfx.core

import org.scalagfx.math._
import org.scalagfx.opengl._
import org.scalagfx.io.Path

import org.lwjgl._
import org.lwjgl.input._
import org.lwjgl.opengl._
import GL11._
import GL12._
import GL15._
import GL20._

//--------------------------------------------------------------------------------------------------
//   Z O O M   C A M E R A 
//--------------------------------------------------------------------------------------------------

/** Companion object for ZoomCamera. */
object ZoomCamera
{
  /* Create an initial camera.
   *
   * @param mode The OpenGL window display mode.
   * @param zoom The level of zoom (power of 2). */ 
  def apply(mode: DisplayMode, zoom: Double = 0.0): ZoomCamera = {
    val camera = OrthoCamera(mode, -1.0, 1.0)
    val factor = camera.bbox.range.toVec2d / Index2i(mode.getWidth, mode.getHeight).toVec2d
    new ZoomCamera(camera, zoom, Pos2d(0.0), factor)
  }
}

/** A 2D orthogonal camera which can be positioned and zoomed. 
  *
  * @constructor Create a new camera.
  * @param camera The 3D orthogonal view transformation.
  * @param zoom The level of zoom (power of 2).
  * @param pos The camera viewing position.
  * @param factor The scaling factor from window to camera space. */ 
class ZoomCamera(val camera: OrthoCamera, val zoom: Double, val pos: Pos2d, val factor: Vec2d)
{
  /** The current camera scaling factor based on zoom level. */
  val scale = scala.math.pow(2.0, zoom) 

  /** The bounding box of the viewable area in world space. */ 
  val bbox = camera.bbox.toBBox2d.map((c: Pos2d) => (pos * -1.0) + (c.toVec2d / scale))

  /** The combined view and projection transformation. */ 
  val xform = 
    (camera.project * 
     Matrix44d.scale(Vector3d(scale, scale, 1.0)) * 
     Matrix44d.translate(pos.toVector3d))

  /** Create a new camera with update position and zoom only. */ 
  def update(newZoom: Double, newPos: Pos2d) = 
    new ZoomCamera(camera, newZoom, newPos, factor)
}


//--------------------------------------------------------------------------------------------------
//   Z O O M   C A M E R A   T H I N G
//--------------------------------------------------------------------------------------------------

/** Companion object for ZoomCameraThing. */
object ZoomCameraThing
{
  // Load and compile shared shaders.
  private lazy val prog = {
    val dir = Path("/com/stellargame/core/shaders")
    import ShaderType._
    ShaderProgram(List(Shader(Vertex,   dir + "debug.vert"), 
                       Shader(Fragment, dir + "debug.frag")), 
                  List("MvpMatrix", "DebugColor"))
  }

  // Get attribute location for Model-View-Projection matrix. 
  private lazy val mvpMatrixLoc = prog.uniforms("MvpMatrix")

  // Get attribute location for the debugging color. 
  private lazy val debugColorLoc = prog.uniforms("DebugColor")
}
