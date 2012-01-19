package org.scalagfx.core

//--------------------------------------------------------------------------------------------------
//  L O C A L
//--------------------------------------------------------------------------------------------------

/** Thrown when the application encounters a fatal exception. */ 
class ApplicationException(message: String) extends Exception(message)

/** Kinds of operating systems. */
object OsType extends Enumeration {
  val Linux, MacOSX, Windows = Value
}

/** A collection of information about the local system running the application. */
object Local {

  /** The operating system (OsType) running the application. */ 
  val os: OsType.Value = {
    sys.props("os.name") match {
      case "Linux" => OsType.Linux
      case "Mac OS X" => OsType.MacOSX
      case "Windows XP" | "Windows 2000" | "Windows 2003" | "Windows NT" | "Windows Vista" |
           "Windows 7" => OsType.Windows
      case n => throw new ApplicationException("Operating system (" + n + ") is not supported!")
    }
  }
}
