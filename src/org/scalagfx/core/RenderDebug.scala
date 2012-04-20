// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.core

//--------------------------------------------------------------------------------------------------
//   R E N D E R   D E B U G
//--------------------------------------------------------------------------------------------------

/** A set of global render debugging controls. */ 
object RenderDebug
{
  /** An alternate debugging camera to use when rendering. */ 
  var camera: Option[ZoomCamera] = None

  /** Whether any debugging geometry should be rendered. */ 
  def showAny = showBoundingBoxes || showInView || showInZoomRange

  /** Whether to render the bounding boxes of entities. */ 
  var showBoundingBoxes = false
  
  /** Whether to render indicators of when an entity is within the normal viewing bounds. */
  var showInView = false

  /** Whether to render indicators that the entity should be visible at the current zoom level. */
  var showInZoomRange = false
}
