package org.scalagfx.opengl

import org.scalagfx.io.{Path,Resources}

import org.lwjgl._
import org.lwjgl.opengl.{GL11,GL20,GL32}
import GL11._
import GL20._
import GL32._

import java.net.URL

//--------------------------------------------------------------------------------------------------
//   S H A D E R S                                                        
//--------------------------------------------------------------------------------------------------

/** Types of GLSL shaders. */ 
object ShaderType extends Enumeration {
  val Vertex, Geometry, Fragment = Value
}

/** Factory for Shader. */ 
object Shader 
{
  /** Create a new GLSL shader by loading and compiling its source code.
    *
    * @param shaderType The type of GLSL shader. 
    * @param path The path of the file within the application JAR containing the shader source
    * code. */
  def apply(shaderType: ShaderType.Value, path: Path): Shader = {
    import ShaderType._
    val stype = shaderType match {
      case Vertex   => GL_VERTEX_SHADER
      case Geometry => GL_GEOMETRY_SHADER
      case Fragment => GL_FRAGMENT_SHADER
    }
    new Shader(stype, path) 
  }
}

/** A GLSL shader. */ 
class Shader(shaderType: Int, path: Path) 
{
  val shaderID = {
    import java.io.{BufferedReader,InputStreamReader}
    val shader = glCreateShader(shaderType);
    if(shader == 0) {
      println("ERROR - Failed to create GLSL shader!")
      0 
    }
    else {
      try {
        val istream = Resources.getResource(path.toString)
        val in = new BufferedReader(new InputStreamReader(istream))
        def readSource: String = {
          val line = in.readLine
          if(line == null) ""
          else line + "\n" + readSource
        }
          
        glShaderSource(shader, readSource) 
        glCompileShader(shader)
        if(glGetShader(shader, GL_COMPILE_STATUS) == GL_TRUE) {
          println("GLSL shader compiled: " + path)
          shader
        }
        else {
          println("ERROR - GLSL shader compile failed:\n" + 
                  glGetShaderInfoLog(shader, glGetShader(shader, GL_INFO_LOG_LENGTH)))
          0
        }
      }
      catch { 
        case ex: Exception => {
          println("ERROR - Unable to read GLSL shader source from: " + path + "\n" + 
                  ex.getMessage)
          0
        }
      }
    }
  }
}
  

//--------------------------------------------------------------------------------------------------
//   S H A D E R   P R O G R A M                                                            
//--------------------------------------------------------------------------------------------------

/** Factory for ShaderProgram. */ 
object ShaderProgram 
{
  /** Create a new GLSL shader program by binding shader attributes and linking the shaders.
    *
    * @param shaders List of GLSL shaders which make up the program.
    * @param unames The names of the uniform variables defined in the shaders. */
  def apply(shaders: List[Shader], unames: List[String]): ShaderProgram = 
    new ShaderProgram(shaders, unames)
}

/** A set of GLSL shaders which together make up a shader program. */
class ShaderProgram(val shaders: List[Shader], unames: List[String]) 
{
  /** Link the shaders storing the shader program handle. */ 
  val programID: Int = {
    val pid = glCreateProgram
    if(pid == 0) {
      println("ERROR - Failed to create GLSL shader program!")
      freeResources(pid)
    }
    else {
      // Attach the shaders to the program.
      for(shader <- shaders)
        glAttachShader(pid, shader.shaderID)

      // Link shaders to build the shader program.
      glLinkProgram(pid)
      if(glGetProgram(pid, GL_LINK_STATUS) == GL_TRUE) {
        println("Shader program linked successfully.")
        pid
      }
      else {
        println("ERROR - GLSL shader program link failed:\n" + 
                glGetProgramInfoLog(pid, glGetProgram(pid, GL_INFO_LOG_LENGTH)))
        freeResources(pid)
      }
    }
  }

  /** A map of uniform variable names to IDs. */
  val uniforms = {
    def f(m: Map[String,Int], u: String) = m.updated(u, glGetUniformLocation(programID, u))
    ((Map(): Map[String,Int]) /: unames)(f)
  }
 	
  /** Free up any OpenGL resources used by the shader program.
    *
    * Should the programID and Shader.shaderID be "vars" and not vals" to prevent this from being
    * called twice?  Also how should we cleanup resources after successfully using some shaders
    * but no longer needing them?
    *
    * @param pid The program handle.
    * @returns Always 0.*/
  private def freeResources(pid: Int): Int = { 
    for(shader <- shaders) {
      if(pid != 0) 
        glDetachShader(pid, shader.shaderID)
      glDeleteShader(shader.shaderID)
    }
    if(pid != 0) 
      glDeleteProgram(pid)
    0
  }
}
