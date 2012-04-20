// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.houdini.geo.attr

import org.scalagfx.math.{Index2i, Index3i, Vec2d, Vec3d}

//--------------------------------------------------------------------------------------------------
//   P R I M I T I V E   C L A S S E S 
//--------------------------------------------------------------------------------------------------

class PrimitiveIntAttr private (val name: String, val default: Int)
  extends IntAttr with PrimitiveAttr
  
object PrimitiveIntAttr 
{
  /** Create a new GEO Primitive Attribute of Int value type. */
  def apply(name: String, default: Int) = new PrimitiveIntAttr(name, default)
}
  
class PrimitiveIndex2iAttr private (val name: String, val default: Index2i)
  extends Index2iAttr with PrimitiveAttr
  
object PrimitiveIndex2iAttr 
{
  /** Create a new GEO Primitive Attribute of Index2i value type. */
  def apply(name: String, default: Index2i) = new PrimitiveIndex2iAttr(name, default)
}
  
class PrimitiveIndex3iAttr private (val name: String, val default: Index3i)
  extends Index3iAttr with PrimitiveAttr

object PrimitiveIndex3iAttr 
{
  /** Create a new GEO Primitive Attribute of Index3i value type. */
  def apply(name: String, default: Index3i) = new PrimitiveIndex3iAttr(name, default)
}
  
class PrimitiveFloatAttr private (val name: String, val default: Double)
  extends FloatAttr with PrimitiveAttr
  
object PrimitiveFloatAttr 
{
  /** Create a new GEO Primitive Attribute of Double value type. */
  def apply(name: String, default: Double) = new PrimitiveFloatAttr(name, default)
}
  
class PrimitiveVec2dAttr private (val name: String, val default: Vec2d)
  extends Vec2dAttr with PrimitiveAttr
  
object PrimitiveVec2dAttr 
{
  /** Create a new GEO Primitive Attribute of Vec2d value type. */
  def apply(name: String, default: Vec2d) = new PrimitiveVec2dAttr(name, default)
}
  
class PrimitiveVec3dAttr private (val name: String, val default: Vec3d)
  extends Vec3dAttr with PrimitiveAttr

object PrimitiveVec3dAttr 
{
  /** Create a new GEO Primitive Attribute of Vec3d value type. */
  def apply(name: String, default: Vec3d) = new PrimitiveVec3dAttr(name, default)
}
  
class PrimitiveStringAttr private (val name: String, val default: List[String])
  extends StringAttr with PrimitiveAttr
  
object PrimitiveStringAttr 
{
  /** Create a new GEO Primitive Attribute of String value type. */
  def apply(name: String, default: List[String]) = new PrimitiveStringAttr(name, default)
}
  
