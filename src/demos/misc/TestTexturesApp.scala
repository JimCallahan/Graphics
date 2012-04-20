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
import GL13._
import GL20._
import GL30._
import GL33._

import java.nio.FloatBuffer

object TestTexturesApp 
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


    //---------------------------------------------------------------------------------------------

    val dir = Path("/demos/misc/shaders")

    // Load and compile shaders.
    val simpleProg = {
      import ShaderType._
      ShaderProgram(List(Shader(Vertex,   dir + "texture.vert"), 
                         Shader(Fragment, dir + "texture.frag")), 
                    List("MvpMatrix", "Texture"))
    }

    val coloredProg = {
      import ShaderType._
      ShaderProgram(List(Shader(Vertex,   dir + "colored-texture.vert"), 
                         Shader(Fragment, dir + "colored-texture.frag")), 
                    List("MvpMatrix", "Texture"))
    }


    //---------------------------------------------------------------------------------------------

    // The hardware texture unit where texture/sampler are bound.
    val texUnit = 3

    // Texture and sampler.
    val texture = TestTexture2D(1024, texUnit)
    val sampler = ClampedMipmapSampler(texUnit)


    //---------------------------------------------------------------------------------------------

    // One texture mapped quad.
    val simpleVAO = {
      val verts = {
        val buf = BufferUtils.createFloatBuffer(4*2)  // 4 verts, 2 XY

        Vec2d(-1.0) -> buf
        Vec2d(-1.0, 1.0) -> buf
        Vec2d(1.0, -1.0) -> buf
        Vec2d(1.0) -> buf

        buf.rewind

        val vbo = FloatVBO(2) 
        vbo.load(buf)
        buf.clear 

        vbo
      }

      val coords = {
        val buf = BufferUtils.createFloatBuffer(4*2)  // 4 verts, 2 UV

        Vec2d(0.0) -> buf
        Vec2d(0.0, 1.0) -> buf
        Vec2d(1.0, 0.0) -> buf
        Vec2d(1.0) -> buf

        buf.rewind

        val vbo = FloatVBO(2) 
        vbo.load(buf)
        buf.clear 

        vbo
      }

      val indices = {
        val buf = BufferUtils.createIntBuffer(2*3)  // 2 tris, 3 indices

        buf.put(0)
        buf.put(1)
        buf.put(2)

        buf.put(2)
        buf.put(1)
        buf.put(3)

        buf.rewind

        val ibo = IntIBO()
        ibo.load(buf)
        buf.clear 

        ibo
      }

      VertexArrayObject(List(verts, coords), indices)
    }

    // A number of randomly placed and sized texture mapped quads.
    val numRandomQuads = 4096
    val randomVAO = {
      val verts = {
        val bbox = cam.camera.bbox.toBBox2d
        val size = bbox.range.x * 0.001
        val buf = BufferUtils.createFloatBuffer(numRandomQuads*4*2)  // 4 verts, 2 XY
        for(i <- 0 until numRandomQuads) {
          val p = bbox.randomPos 
          val d = Vec2d(size + size*scala.math.random*32.0)
          (p - d) -> buf
          (p + d.newY(0.0) - d.newX(0.0)) -> buf
          (p - d.newY(0.0) + d.newX(0.0)) -> buf
          (p + d) -> buf
        }
        buf.rewind

        val vbo = FloatVBO(2) 
        vbo.load(buf)
        buf.clear 

        vbo
      }

      val coords = {
        val buf = BufferUtils.createFloatBuffer(numRandomQuads*4*2)  // 4 verts, 2 UV
        for(_ <- 0 until numRandomQuads) {
          Vec2d(0.0) -> buf
          Vec2d(0.0, 1.0) -> buf
          Vec2d(1.0, 0.0) -> buf
          Vec2d(1.0) -> buf
        }          
        buf.rewind

        val vbo = FloatVBO(2) 
        vbo.load(buf)
        buf.clear 

        vbo
      }

      val colors = {
        val buf = BufferUtils.createFloatBuffer(numRandomQuads*4*3)  // 4 verts, 3 RGB
        for(_ <- 0 until numRandomQuads) {
          val c = Vec3d.random
          for(_ <- 0 until 4) 
            c -> buf
        }
        buf.rewind

        val vbo = FloatVBO(3) 
        vbo.load(buf)
        buf.clear 

        vbo
      }

      val indices = {
        val buf = BufferUtils.createIntBuffer(numRandomQuads*2*3)  // 2 tris, 3 indices
        for(i <- 0 until numRandomQuads) {
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

      VertexArrayObject(List(verts, coords, colors), indices)
    }


    //---------------------------------------------------------------------------------------------

    // Make texture/sampler active.
    texture.bind
    sampler.bind


    //---------------------------------------------------------------------------------------------

    // Timing statistics. 
    val stats = TimingStats()

    // The input/render loop.
    var geometry = 1
    while (!Keyboard.isKeyDown(Keyboard.KEY_ESCAPE) && !Display.isCloseRequested()) {
      val now = stats.update
    
      // Change which geometry to display.
      if(Keyboard.isKeyDown(Keyboard.KEY_1)) 
        geometry = 1
      else if(Keyboard.isKeyDown(Keyboard.KEY_2)) 
        geometry = 2
      
      /** Update camera. */
      cam = ZoomController.control(cam) 

      // Clear the screen.
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

      // Chose shader program to use.
      val prog = geometry match {
        case 1 => simpleProg
        case 2 => coloredProg
      }
      glUseProgram(prog.programID)
      glUniform1i(prog.uniforms("Texture"), texUnit) 

      // Set Model-View-Projection matrix. 
      glUniformMatrix4(prog.uniforms("MvpMatrix"), false, cam.xform.toNativeFloats) 

      // Draw the geometry using VAOs.
      geometry match {
        case 1 => {
          simpleVAO.bind
          glDrawRangeElements(GL_TRIANGLES, 0, 6, 6, 
                              simpleVAO.ibo.dataType, 0) 
        }

        case 2 => {
          randomVAO.bind
          glDrawRangeElements(GL_TRIANGLES, 0, numRandomQuads*6, numRandomQuads*6, 
                              randomVAO.ibo.dataType, 0) 
        }
      }

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
      print("Welcome to TestTextures!\n" +
            "--------------------------------------\n" +
            "  Quit = Esc\n" + 
            "--------------------------------------\n")

      TestTexturesApp.run      

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
