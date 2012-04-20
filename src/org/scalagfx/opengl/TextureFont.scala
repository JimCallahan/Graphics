// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.opengl

import org.scalagfx.io.{Path,Resources}
import org.scalagfx.math._

import org.lwjgl._
import org.lwjgl.opengl._
import GL11._
import GL12._
import GL15._
import GL21._

import java.nio._

//--------------------------------------------------------------------------------------------------
//   T E X T U R E   F O N T                                                          
//--------------------------------------------------------------------------------------------------

/** Metric information about a specific textured glyph.
  *
  * @constructor Create new metric information for a textured glyph.
  * @param char The character represented by the glyph.
  * @param index The index of the glyph cell within the texture image.
  * @param advance The amount to advance the position (in points) after rendering the glyph. */ 
class FontGlyph(val char: Char, val index: Index2i, val advance: Double) {
  /** Convert to a string representation. */
  override def toString() = 
    "Glyph(" + char.toInt + ", " + index + ", " + advance + ")"
}


//--------------------------------------------------------------------------------------------------

/** A factory for TexFont. */
object TextureFont 
{
  /** Create a new font from TrueType specification file.
    *
    * @param fontSpec The location of the TrueType font specification file within the application JAR. 
    * @param textureUnit The ID of the hardware texture unit where the texture will be bound. */ 
  def apply(fontSpec: Path, textureUnit: Int): TextureFont = 
    TextureFont(fontSpec, 35.0, Pos2d(0.075, 0.75), textureUnit)
  
  /** Create a new font from TrueType specification file.
    *
    * @param fontSpec The location of the TrueType font specification file within the application JAR.
    * @param pointSize The size to render the glyphs in points.
    * @param cellPos The position within a cell to render each glyph. 
    * @param textureUnit The ID of the hardware texture unit where the texture will be bound. */ 
  def apply(fontSpec: Path, pointSize: Double, cellPos: Pos2d, textureUnit: Int): TextureFont = {
    import scala.math.{ceil,log,pow,sqrt}
    import java.awt.{Font,RenderingHints}
    import java.awt.image.BufferedImage

    // Load the TrueType font and size it appropriately.    
    val font = Font.createFont(Font.TRUETYPE_FONT, Resources.getResource(fontSpec.toString))
    val fontSized = font.deriveFont(pointSize.toFloat)

    // Determine which characters can be displayed. 
    val legal: Array[Char] = 
      (for(i <- 0 until 65536; c = i.toChar; if(fontSized.canDisplay(c))) yield { c }).toArray
    
    // Compute the minimum number of cells in a square texture map required to display the legal
    // character glyphs.  Glyphs are rendered into cells with a height twice the width of the cell.
    val size = Index2i(ceil(sqrt(legal.size.toDouble)).toInt)

    // Figure out the nearest power of two texture map resolution.
    val res = pow(2.0, ceil(log(size.x.toDouble * pointSize) / log(2.0))).toInt

    println("res = " + res)

    // The layout of glyphs within the texture map.
    val layout = Space2d(BBox2d(Pos2d.origin, Index2i(res).toPos2d), size) 

    println("layout = " + layout)

    // Create an image to render into.
    val imgSize = layout.bmax.toIndex2i
    val img = new BufferedImage(imgSize.x, imgSize.y, BufferedImage.TYPE_INT_ARGB)

    println("imgSize = " + imgSize)

    // Set up the graphics context for anti-aliased text.
    val gfx = img.createGraphics
    gfx.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    // Render each character, collecting metrics for each glyph rendered.
    val gs = 
      for(y <- 0 to size.y; x <- 0 to size.x; 
          i = y*size.x+x; if(i < legal.size); 
          c = legal(i); idx = Index2i(x, y)) yield {
        val cell = layout.cellBounds(idx)
        val pos = cell.position(cellPos)
        val gvec = fontSized.createGlyphVector(gfx.getFontRenderContext, Array(c)) 
        gfx.drawGlyphVector(gvec, pos.x.toFloat, pos.y.toFloat)       

        new FontGlyph(c, idx, gvec.getGlyphMetrics(0).getAdvance.toDouble)
      }

    // Build the glyph table.
    val glyphs = ((Map(): Map[Char,FontGlyph]) /: gs)((m, g: FontGlyph) => m.updated(g.char, g))
    
    // DEBUG
    import javax.imageio.ImageIO 
    import java.io.File
    ImageIO.write(img, "png", Path("c:/Users/jim/tmp/font-test.png").toFile)
    // DEBUG

    // Copy data into a native buffer. 
    val buf = BufferUtils.createByteBuffer(imgSize.x*imgSize.y*4)  // RGBA (premultiplied)
    val raster = img.getRaster
    for(y <- 0 until img.getHeight; 
        x <- 0 until img.getWidth) { 
      val alpha = raster.getSample(x, img.getHeight-y-1, 3).toByte
      buf.put(alpha); buf.put(alpha); buf.put(alpha); buf.put(alpha)
    }
    buf.rewind

    val tfont = new TextureFont(font.getName, pointSize, glyphs, layout, cellPos, textureUnit) 
    tfont.load(buf)
    tfont
  }

}

/** A set of textured glyphs corresponding to a specific font TrueType font.
  *
  * @constructor Create a new textured font.
  * @param name The name of the TrueType font.
  * @param pointSize The size to render the glyphs in points.
  * @param glyphs The metrics used to render the set of textured glyphs.
  * @param layout The layout of glyph cells within the texture image. 
  * @param cellPos The position within a cell to render each glyph. 
  * @param textureUnit The ID of the hardware texture unit where this texture will be bound. */ 
class TextureFont(val name: String, val pointSize: Double, val glyphs: Map[Char,FontGlyph],
                  val layout: Space2d, val cellPos: Pos2d, textureUnit: Int)
  extends Texture2D(layout.bmax.toIndex2i, GL_RGBA, textureUnit)
{
  /** Load texture data and generate mipmaps. */ 
  def load(data: ByteBuffer) {
    load(GL_RGBA, data)
  }


  // .. 
}
