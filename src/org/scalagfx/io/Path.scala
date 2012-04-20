// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.io

import org.scalagfx.core.{Local,OsType}
import java.io.File

//--------------------------------------------------------------------------------------------------
//  P A T H   
//--------------------------------------------------------------------------------------------------

/** Factory for Path. */
object Path {
  //------------------------------------------------------------------------------------------------
  //   C R E A T I O N                                                                     
  //------------------------------------------------------------------------------------------------

  /** Create a path from a localized file system path string, validating it in the process. */ 
  private def validate(local: String): Path = {
    val s = local.replace('\\', '/')
    val (prefix, rest) = 
      s.take(3).toList match { 
        case x @ List(drive @ _, ':', '/') if drive.isLetter => (Some(s.take(3)), s.drop(3))
        case List('/', '/', _*) => (Some(s.take(2)), s.drop(2))
        case List('/', _*) => (Some(s.take(1)), s.drop(1))
        case _ => (None, s)
      }

    new Path(prefix, rest.split('/').toList.filter(x => !x.isEmpty && (x != ".")))
  }

  /** Create a path from a file. */ 
  def apply(file: File): Path = validate(file.getPath)

  /** Create a path from a localized string. */ 
  def apply(path: String): Path = validate(path)

  /** Create a path from a prefix and set of path components.
    *
    * @param prefix Optional prefix of absolute paths: "/" (Unix-like), "//" (UNC) or 
    *               "X:/" (Windows, where X is a drive letter)
    * @param comps List of zero or more path components without delimiters. */ 
  def apply(prefix: Option[String], comps: String*): Path = {
    prefix match {
      case Some(p) => 
        p.toList match {
          case x @ List(drive @ _, ':', '/') if drive.isLetter => ()
          case List('/', '/') | List('/') => ()
          case _ => 
            throw new IllegalArgumentException(
              "If supplied, the path prefix must be one of \"/\", \"//\" or \"X:/\" " + 
              "(where X is a drive letter!")
        }
      case _ => () 
    }

    if(comps.exists(_.contains('\\')))
      throw new IllegalArgumentException(
        "Path components cannot contain the Windows specific delimeter '\' character!")

    if(comps.exists(_.contains('/')))
      throw new IllegalArgumentException(
        "Path components cannot contain the delimeter '/' character!")

    val cs = comps.toList.filter(x => !x.isEmpty && (x != "."))
    prefix match {
      case None if (cs.isEmpty) => 
        throw new IllegalArgumentException(
          "If no prefix is supplied, then there must be at least one valid path component!")
      case _ => new Path(prefix, cs)
    }
  }
    
  /** Create a new path by concatenating a set path components onto an existing path. 
    *
    * @param path The root path.
    * @param comps List of zero or more path components without delimiters. */ 
  def apply(path: Path, comps: String*): Path = 
    Path(path.prefix, (path.comps ::: comps.toList):_*)

  /** Create a new path by concatenating a set of existing paths.
    *
    * The prefix of the newly create path will be taken from the first path given, all other
    * prefixes (if any) will be ignored. */ 
  def apply(paths: Path*): Path = {
    paths.toList match {
      case List() => throw new IllegalArgumentException("At least one path must be supplied!")
      case List(p) => p
      case List(p, ps @ _*) => new Path(p.prefix, (p.comps /: ps)(_ ::: _.comps))
    }
  }
}

/** An operating system independent file system path representation.
  *
  * Unlike java.io.File, paths maintain a consistent representation for all operating systems so
  * that program logic can be written without consideration of the particulars of the local
  * environment.  Paths are easily convertible to/from File, to make them easy to use with all
  * of the standard I/O classes and methods.  Paths also have some enhanced functionality for
  * concatenation, traversal and manipulation.
  *
  * The major difference though is that toString returns a path which always uses the Unix-like
  * convention of separating paths with the '/' character, regardless of whether the path was
  * specified on Linux, MacOSX or Windows.  A localized string representation can be obtained using
  * the toLocalString methods, although usually this is not needed since most places that require
  * a localized path take the File type instead of String.
  *
  * A Path consists of an optional prefix and a list of path components.  If a prefix is present,
  * then the path is absolute and may contain zero or more path components.  Paths without a prefix
  * are relative and must contain at least one path component.  Path components cannot contain a
  * delimeter character ('/' or '\') and cannot be empty.  They may contain any legal file or
  * directory name including the special ".." name.
  *
  * Paths should be constructed using the apply methods from the companion Path object which
  * provides enhanced validation as well as many convenient ways of creating.
  * 
  * @constructor Create a new path.
  * @param prefix Optional prefix of absolute paths: "/" (Unix-like), "//" (UNC) or 
  *               "X:/" (Windows, where X is a drive letter)
  * @param comps List of zero or more path components without delimiters. */ 
class Path(val prefix: Option[String], val comps: List[String]) {
  //------------------------------------------------------------------------------------------------
  //   P R E D I C A T E S                                                                  
  //------------------------------------------------------------------------------------------------

  /** Whether the path is absolute. */ 
  val isAbsolute = prefix.isDefined

  /** Whether this path denotes a non-root file/directory. */ 
  val hasName = !comps.isEmpty

  /** Whether this path has a parent directory. */ 
  val hasParent = !comps.isEmpty


  //------------------------------------------------------------------------------------------------
  //  O P S                                                                                 
  //------------------------------------------------------------------------------------------------

  /** The name of the file or directory denoted by this path (if any). */ 
  def name(): String = 
    if(comps.isEmpty)      
      throw new UnsupportedOperationException("A path without components cannot have a name!") 
    else comps.last

  /** The path to the parent directory containing this path (if any) */ 
  def parent(): Path = 
    if(!comps.isEmpty) Path(prefix, comps.dropRight(1):_*)
    else throw new UnsupportedOperationException("A path without components cannot have a parent!")

  /** Concatenate another path with this path. */ 
  def + (that: Path): Path = 
    Path(this, that)

  /** Concatenate a string path component with this path. */ 
  def + (name: String): Path = 
    Path(this, name)


  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Compares this path to the specified value for equality. */
  override def equals(that: Any): Boolean = 
    that match {
      case that: Path => 
        (that canEqual this) && (toString == (that.toString))
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean = 
    that.isInstanceOf[Path]

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 
    toString.hashCode


  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a File. */ 
  def toFile() = new File(toLocalString)

  /** Convert to a String representation localized for the current operating system. */ 
  def toLocalString(): String = toLocalString(Local.os)

  /** Convert to a String representation localized for the given operating system. */ 
  def toLocalString(os: OsType.Value): String = 
    os match {
      case OsType.Windows => toString.replace('/', '\\')
      case _ => toString
    }  

  /** Convert to a String representation. */ 
  override def toString() = {
    val rest = comps match {
      case List(h, t @ _*) => (h /: t)(_ + "/" + _)
      case _ => ""
    }
    prefix match {
      case Some(p) => p + rest
      case None => rest
    }
  }    
}
