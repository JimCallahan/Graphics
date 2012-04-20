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

object TestFontsApp 
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

    // Load and compile shaders.
    val dir = Path("/demos/misc/shaders")

    val normProg = {
      import ShaderType._
      ShaderProgram(List(Shader(Vertex,   dir + "texture.vert"), 
                         Shader(Fragment, dir + "texture.frag")), 
                    List("MvpMatrix", "Texture"))
    }

    val invProg = {
      import ShaderType._
      ShaderProgram(List(Shader(Vertex,   dir + "texture.vert"), 
                         Shader(Fragment, dir + "texture-inverse.frag")), 
                    List("MvpMatrix", "Texture"))
    }

    val debugProg = {
      import ShaderType._
      ShaderProgram(List(Shader(Vertex,   dir + "texture.vert"), 
                         Shader(Fragment, dir + "red-debug.frag")), 
                    List("MvpMatrix", "Texture"))
    }

    
    //---------------------------------------------------------------------------------------------

    // The hardware texture unit where texture/sampler are bound.
    val texUnit = 3

    // Load the font texture.
    val fontTexture = TextureFont(Path("/fonts/enigmatic/Enigma.ttf"), texUnit)

    // Create texture sampler.
    val sampler = ClampedMipmapSampler(texUnit)


    //---------------------------------------------------------------------------------------------

    // A single quad with the whole font texture on it.
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


    // Individual quads for each character in the font.
    val numChars = fontTexture.glyphs.size
    println("Characters in Font: " + numChars)
    
    val charsVAO = {
      val verts = {
        val buf = BufferUtils.createFloatBuffer(numChars*4*2)  // 4 verts, 2 XY
        val fr = Frame2d.translate(Vec2d(-1.0)) concat Frame2d.scale(2.0) 
        for(g <- fontTexture.glyphs.values) {
          val cell = fontTexture.layout.cellCoords(g.index)
          val half = cell.grow(cell.range / -4.0)
          val mn = fr xform half.bmin
          val mx = fr xform half.bmax

          mn -> buf
          Vec2d(mn.x, mx.y) -> buf
          Vec2d(mx.x, mn.y) -> buf
          mx -> buf
        }
        buf.rewind
        
        val vbo = FloatVBO(2) 
        vbo.load(buf)
        buf.clear 

        vbo
      }

      val coords = {
        val buf = BufferUtils.createFloatBuffer(numChars*4*2)  // 4 verts, 2 UV
        for(g <- fontTexture.glyphs.values) {
          val cell = fontTexture.layout.cellCoords(g.index)
          val mn = cell.bmin
          val mx = cell.bmax
          
          mn -> buf
          Vec2d(mn.x, mx.y) -> buf
          Vec2d(mx.x, mn.y) -> buf
          mx -> buf
        }          
        buf.rewind

        val vbo = FloatVBO(2) 
        vbo.load(buf)
        buf.clear 

        vbo
      }

      val indices = {
        val buf = BufferUtils.createIntBuffer(numChars*2*3)  // 2 tris, 3 indices
        for(i <- 0 until numChars) {
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

      VertexArrayObject(List(verts, coords), indices)
    }

    val (dialogVAO, dialogSize) = {
      val dialog = 
        List("The boy has the capacity to change.",
             "The boy has the capacity to do something decent with his life.",
             "",
             "Excuse me, Ken.",
             "I have the capacity to change.",
             "",
             "Yeah, you do.",
             "You've the capacity to get fucking worse!",
             "",
             "Yeah, now I'm getting down to it!",
             "",
             "Harry, let's face it.",
             "And I'm not being funny, I mean no disrespect, but you're a cunt.",
             "You're a cunt now, you've always been a cunt.",
             "And the only thing that's gonna change is you're gonna become an even bigger cunt.",
             "Maybe have some more cunt kids.",
             "",
             "Leave my kids fucking out of it.",
             "What have they done?",
             "You fucking retract that bit about my cunt fucking kids!",
             "",
             "I retract that bit about your cunt fucking kids.",
             "",
             "Insulting my fucking kids!",
             "That's going overboard, mate!",
             "",
             "I retracted it, didn't I?",
             "Still leaves you being a cunt.",
             "",
             "Yeah, I fucking got that.")
             
      val size = dialog.filter(_.size > 0).flatten.filter(fontTexture.glyphs.contains).size
      val scale = dialog.size.toDouble * fontTexture.pointSize
      val fr = Frame2d.scale(2.0 / scale)
      val verts = {
        val buf = BufferUtils.createFloatBuffer(size*4*2)  // 4 verts, 2 XY
        var left = Pos2d(-0.5, 0.5) * scale
        for(line <- dialog) {
          var pos = left
          for(c <- line) {
            fontTexture.glyphs.lift(c) match {
              case Some(g) => {
                //println(g.char + " " + g)
                val mn = fr xform pos
                val mx = fr xform (pos + fontTexture.pointSize)

                mn -> buf
                Vec2d(mn.x, mx.y) -> buf
                Vec2d(mx.x, mn.y) -> buf
                mx -> buf 
                
                pos = pos + Vec2d(g.advance, 0.0)
              }
              case _ =>
            }
          }
          
          left = left - Vec2d(0.0, fontTexture.pointSize)
        }
        buf.rewind
        
        val vbo = FloatVBO(2) 
        vbo.load(buf)
        buf.clear 

        vbo
      }

      val coords = {
        val buf = BufferUtils.createFloatBuffer(size*4*2)  // 4 verts, 2 UV
        for(line <- dialog; c <- line) {
          fontTexture.glyphs.lift(c) match {
            case Some(g) => {
              val cell = fontTexture.layout.cellCoords(g.index)
              val mn = cell.bmin
              val mx = cell.bmax
          
              mn -> buf
              Vec2d(mn.x, mx.y) -> buf
              Vec2d(mx.x, mn.y) -> buf
              mx -> buf
            }
            case _ =>
          }
        }          
        buf.rewind

        val vbo = FloatVBO(2) 
        vbo.load(buf)
        buf.clear 

        vbo
      }

      val indices = {
        val buf = BufferUtils.createIntBuffer(size*2*3)  // 2 tris, 3 indices
        for(i <- 0 until size) {
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

      (VertexArrayObject(List(verts, coords), indices), size)
    }

    //---------------------------------------------------------------------------------------------

    // Make texture/sampler active.
    fontTexture.bind
    sampler.bind


    //---------------------------------------------------------------------------------------------

    // Timing statistics. 
    val stats = TimingStats()

    // Mapping of keyboard events to geometry modes.
    val keyMap = {
      import Keyboard._
      Map(KEY_1 -> 1, KEY_2 -> 2, KEY_3 -> 3, KEY_4 -> 4, KEY_5 -> 5)
    } 

    // The input/render loop.
    var geometry = 1
    while (!Keyboard.isKeyDown(Keyboard.KEY_ESCAPE) && !Display.isCloseRequested()) {
      val now = stats.update

      // Change which geometry to display.
      for(key <- keyMap.keys) {
        if(Keyboard.isKeyDown(key)) 
          geometry = keyMap(key)
      }

      /** Update camera. */
      cam = ZoomController.control(cam) 

      // Clear the screen.
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

      // Chose shader program to use.
      val prog = geometry match {
        case 1|2|5 => normProg
        case 3     => invProg
        case 4     => debugProg
      }

      // Use the program for shading. 
      glUseProgram(prog.programID)
      glUniform1i(prog.uniforms("Texture"), texUnit)  

      // Set Model-View-Projection matrix. 
      glUniformMatrix4(prog.uniforms("MvpMatrix"), false, cam.xform.toNativeFloats)     

      // Draw the geometry using VAOs.
      geometry match {
        case 1 => {
          simpleVAO.bind
          glDrawRangeElements(GL_TRIANGLES, 0, 6, 6, simpleVAO.ibo.dataType, 0) 
        }

        case 2|3|4 => {          
          charsVAO.bind
          glDrawRangeElements(GL_TRIANGLES, 0, numChars*6, numChars*6, charsVAO.ibo.dataType, 0)
        }
        
        case 5 => {          
          dialogVAO.bind
          glDrawRangeElements(GL_TRIANGLES, 0, dialogSize*6, dialogSize*6, dialogVAO.ibo.dataType, 0)
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
      print("Welcome to TestFonts!\n" +
            "--------------------------------------\n" +
            "  Quit = Esc\n" + 
            "--------------------------------------\n")

      TestFontsApp.run      

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
