// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.houdini.geo.attr

import org.scalagfx.math.{Index2i, Index3i, Vec2d, Vec3d}

//--------------------------------------------------------------------------------------------------
//   V E R T E X   C L A S S E S 
//--------------------------------------------------------------------------------------------------

class VertexIntAttr private (val name: String, val default: Int)
  extends IntAttr with VertexAttr
  
object VertexIntAttr 
{
  /** Create a new GEO Vertex Attribute of Int value type. */
  def apply(name: String, default: Int) = new VertexIntAttr(name, default)
}
  
class VertexIndex2iAttr private (val name: String, val default: Index2i)
  extends Index2iAttr with VertexAttr
  
object VertexIndex2iAttr 
{
  /** Create a new GEO Vertex Attribute of Index2i value type. */
  def apply(name: String, default: Index2i) = new VertexIndex2iAttr(name, default)
}
  
class VertexIndex3iAttr private (val name: String, val default: Index3i)
  extends Index3iAttr with VertexAttr

object VertexIndex3iAttr 
{
  /** Create a new GEO Vertex Attribute of Index3i value type. */
  def apply(name: String, default: Index3i) = new VertexIndex3iAttr(name, default)
}
  
class VertexFloatAttr private (val name: String, val default: Double)
  extends FloatAttr with VertexAttr
  
object VertexFloatAttr 
{
  /** Create a new GEO Vertex Attribute of Double value type. */
  def apply(name: String, default: Double) = new VertexFloatAttr(name, default)
}
  
class VertexVec2dAttr private (val name: String, val default: Vec2d)
  extends Vec2dAttr with VertexAttr
  
object VertexVec2dAttr 
{
  /** Create a new GEO Vertex Attribute of Vec2d value type. */
  def apply(name: String, default: Vec2d) = new VertexVec2dAttr(name, default)
}
  
class VertexVec3dAttr private (val name: String, val default: Vec3d)
  extends Vec3dAttr with VertexAttr

object VertexVec3dAttr 
{
  /** Create a new GEO Vertex Attribute of Vec3d value type. */
  def apply(name: String, default: Vec3d) = new VertexVec3dAttr(name, default)
}
  
class VertexStringAttr private (val name: String, val default: List[String])
  extends StringAttr with VertexAttr
  
object VertexStringAttr 
{
  /** Create a new GEO Vertex Attribute of String value type. */
  def apply(name: String, default: List[String]) = new VertexStringAttr(name, default)
}
  
