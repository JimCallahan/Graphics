// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.houdini.geo.attr

import org.scalagfx.math.{Index2i, Index3i, Vec2d, Vec3d}

//--------------------------------------------------------------------------------------------------
//   P O I N T   C L A S S E S 
//--------------------------------------------------------------------------------------------------

class PointIntAttr private (val name: String, val default: Int)
  extends IntAttr with PointAttr
  
object PointIntAttr 
{
  /** Create a new GEO Point Attribute of Int value type. */
  def apply(name: String, default: Int) = new PointIntAttr(name, default)
}
  
class PointIndex2iAttr private (val name: String, val default: Index2i)
  extends Index2iAttr with PointAttr
  
object PointIndex2iAttr 
{
  /** Create a new GEO Point Attribute of Index2i value type. */
  def apply(name: String, default: Index2i) = new PointIndex2iAttr(name, default)
}
  
class PointIndex3iAttr private (val name: String, val default: Index3i)
  extends Index3iAttr with PointAttr

object PointIndex3iAttr 
{
  /** Create a new GEO Point Attribute of Index3i value type. */
  def apply(name: String, default: Index3i) = new PointIndex3iAttr(name, default)
}
  
class PointFloatAttr private (val name: String, val default: Double)
  extends FloatAttr with PointAttr
  
object PointFloatAttr 
{
  /** Create a new GEO Point Attribute of Double value type. */
  def apply(name: String, default: Double) = new PointFloatAttr(name, default)
}
  
class PointVec2dAttr private (val name: String, val default: Vec2d)
  extends Vec2dAttr with PointAttr
  
object PointVec2dAttr 
{
  /** Create a new GEO Point Attribute of Vec2d value type. */
  def apply(name: String, default: Vec2d) = new PointVec2dAttr(name, default)
}
  
class PointVec3dAttr private (val name: String, val default: Vec3d)
  extends Vec3dAttr with PointAttr

object PointVec3dAttr 
{
  /** Create a new GEO Point Attribute of Vec3d value type. */
  def apply(name: String, default: Vec3d) = new PointVec3dAttr(name, default)
}
  
class PointStringAttr private (val name: String, val default: List[String])
  extends StringAttr with PointAttr
  
object PointStringAttr 
{
  /** Create a new GEO Point Attribute of String value type. */
  def apply(name: String, default: List[String]) = new PointStringAttr(name, default)
}
  

