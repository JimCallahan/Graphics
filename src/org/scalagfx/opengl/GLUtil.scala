package org.scalagfx.opengl

import org.lwjgl._
import org.lwjgl.opengl.{Display,DisplayMode,GL11}
import GL11._

object GLUtil 
{
  /** Create a default OpenGL display. 
    * @param fs Whether the display should be full-screen. */ 
  def createDisplay(fs: Boolean = true) = {
    val mode = Display.getDesktopDisplayMode
    println("Display Mode: [" + mode + "]") 
    if(fs) Display.setDisplayModeAndFullscreen(mode)
    else Display.setDisplayMode(mode)
    Display.create
  }

  /** Print OpenGL version information. */ 
  def printInfo {
    println("OpenGL Version: " + glGetString(GL_VERSION))
  }

  /** Print supported OpenGL extensions. */ 
  def printExtensions {
    println("OpenGL Extensions:")
    for(str <- glGetString(GL_EXTENSIONS).split(" ").sortWith(_.compareTo(_) < 0))
      println("  " + str)
  }
}
