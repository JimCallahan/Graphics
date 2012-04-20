// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package demos.misc

import org.scalagfx.core.{ZoomCamera,ZoomController}
import org.scalagfx.math._
import org.scalagfx.opengl._
import org.scalagfx.io.Path

import org.lwjgl._
import org.lwjgl.input._
import org.lwjgl.opengl._
import GL11._
import GL12._
import GL20._
import GL30._
import GL33._

import java.nio.FloatBuffer

object TestVboShadersApp 
{
  /** Initialize OpenGL and run the input/render loop. */ 
  def run() {

    // Initialize OpenGL.
    var cam = {
      GLUtil.createDisplay(false)
      GLUtil.printInfo

      glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
      Display.setVSyncEnabled(true)

      glShadeModel(GL_SMOOTH)
      glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST)

      glEnable(GL_LINE_SMOOTH)
      glHint(GL_LINE_SMOOTH_HINT, GL_NICEST) 
      
      glEnable(GL_POINT_SMOOTH)
      glHint(GL_POINT_SMOOTH_HINT, GL_NICEST) 
      
      glEnable(GL_BLEND)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

      glPolygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_FILL)
      glEnable(GL11.GL_DEPTH_TEST)

      val mode = Display.getDisplayMode
      glViewport(0, 0, mode.getWidth, mode.getHeight)

      ZoomCamera(mode) 
    }

    // Load and compile shaders.
    val prog = {
      val dir = Path("/demos/misc/shaders")
      import ShaderType._
      ShaderProgram(List(Shader(Vertex, dir + "simple.vert"), 
                         Shader(Fragment, dir + "simple.frag")), 
                    List("MvpMatrix"))
    }

    // Get attribute location for Model-View-Projection matrix. 
    val mvpMatrixLoc = prog.uniforms("MvpMatrix")

    // Create some VBO data: random 2D quads with colors.
    val numQuads = 100000
    val vao = {
      val verts = {
        val bbox = cam.camera.bbox.toBBox2d
        val diag = Pos2d(bbox.range.length)
        val sbox = BBox2d(diag * -1.0, diag) 
        val size = bbox.range.x * 0.005
        val delta = Vec2d(size)
        val dx = delta.newY(0.0)
        val dy = delta.newX(0.0)
        val buf = BufferUtils.createFloatBuffer(numQuads*4*2)  // 4 verts, 2 XY
        for(i <- 0 until numQuads) {
          val p = sbox.randomPos 
          (p - delta) -> buf
          (p + dx - dy) -> buf
          (p - dx + dy) -> buf
          (p + delta) -> buf
        }
        buf.rewind

        val vbo = FloatVBO(2) 
        vbo.load(buf)
        buf.clear 

        vbo
      }

      val colors = {
        import scala.math.random
        val buf = BufferUtils.createFloatBuffer(numQuads*4*3)  // 4 verts, 3 RGB
        for(i <- 0 until numQuads) { 
          val c = i.toDouble / numQuads.toDouble
          Vector3d(0.0, 0.0,   c) -> buf 
          Vector3d(  c, 0.0, 0.0) -> buf 
          Vector3d(0.0,   c, 0.0) -> buf 
          Vector3d(  c,   c, 0.0) -> buf 
        }
        buf.rewind

        val vbo = FloatVBO(3)
        vbo.load(buf)
        buf.clear 

        vbo
      }

      val indices = {
        val buf = BufferUtils.createIntBuffer(numQuads*2*3)  // 2 tris, 3 indices
        for(i <- 0 until numQuads) {
          buf.put(i*4+0)
          buf.put(i*4+1)
          buf.put(i*4+2)

          buf.put(i*4+3)
          buf.put(i*4+2)
          buf.put(i*4+1)
        }
        buf.rewind

        val ibo = IntIBO()
        ibo.load(buf)
        buf.clear 

        ibo
      }

      VertexArrayObject(List(verts, colors), indices)
    }

    // Timing statistics. 
    val stats = TimingStats()

    // Set shader program.
    glUseProgram(prog.programID);

    // The input/render loop.
    var ang = 0.0
    while (!Keyboard.isKeyDown(Keyboard.KEY_ESCAPE) && !Display.isCloseRequested()) {
      val now = stats.update
    
      /** Update camera. */
      cam = ZoomController.control(cam)

      // Clear the screen.
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

      // Set Model-View-Projection matrix. 
      ang = ang + scala.math.Pi*0.001
      val mx = cam.xform * Matrix44d.rotateZ(ang)
      glUniformMatrix4(mvpMatrixLoc, false, mx.toNativeFloats)
      
      // Draw the triangle using VAO.
      vao.bind
      glDrawRangeElements(GL_TRIANGLES, 0, numQuads*6, numQuads*6, vao.ibo.dataType, 0) 

      // Swap buffers.
      Display.update
      Display.sync(60)
    }

    // Report timing statistics.
    stats.report
  }

  /** The top-level entry method. */
  def main(args: Array[String]) {
    try {
      print("Welcome to TestVboShaders!\n" +
            "--------------------------------------\n" +
            "  Quit = Esc\n" + 
            "--------------------------------------\n")

      TestVboShadersApp.run      

      println("Goodbye.")
      sys.exit
    }
    catch {
      case ex: OpenGLException => 
        println("GL ERROR - " + ex.getMessage + "\n" +
                "Stack Trace:\n" + ex.getStackTraceString)
      case ex => 
        println("Uncaught Exception: " + ex.getMessage + "\n" +
                "Stack Trace:\n" + ex.getStackTraceString)
    }
  }
}
