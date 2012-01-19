package org.scalagfx.io

import java.net._
import java.io._
import java.util.jar._

//--------------------------------------------------------------------------------------------------
//   R E S O U R C E S                                                                      
//--------------------------------------------------------------------------------------------------

/** Provides access to data contained in the application JAR. */ 
object Resources
{
  /** 
   * Get the InputStream for reading the data of the name resource from the application JAR.
   * 
   * @param name 
   *   The name of the resource.
   * 
   * @throws IOException
   *   If unable to locate or otherwise acquire the resource input stream.
   */
  def getResource(name: String): InputStream = {
    class Dummy
    val url = (new Dummy).getClass.getResource(name)
    if(url == null) 
      throw new IOException("Unable to find resource (" + name + ")!")
       
    try {
      url.openConnection match {
        case con: JarURLConnection => {
          val jar = con.getJarFile
          val entry = con.getJarEntry
          if(entry == null) 
            throw new IOException(
              "Unable to find JAR entry for resource (" + name + ") in (" + url + ")!")

      	  jar.getInputStream(entry)
        }
      
        case con: URLConnection => con.getInputStream
      }
    }
    catch {
      case ex => 
        throw new IOException(
    	  "Unable get input stream for resource (" + name + ") in (" + url + ")!", ex)
    }
  }
}
