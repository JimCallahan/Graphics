package org.scalagfx.houdini.geo

import org.scalagfx.houdini.geo.attr._
import org.scalagfx.math.{ Index2i, Index3i, Vec2d, Vec3d, Pos3d }

import collection.immutable.{ TreeMap, TreeSet }

import java.io.Writer

//--------------------------------------------------------------------------------------------------
//   G E O   W R I T E R
//--------------------------------------------------------------------------------------------------

/** Writes text format Houdini GEO files. */
class GeoWriter private (val numPoints: Int, val numPrims: Int,
                         val pointGroups: TreeMap[String, Set[Int]], val primGroups: TreeMap[String, Set[Int]],
                         val pointAttrs: TreeMap[String, PointAttr],
                         val vertexAttrs: TreeMap[String, VertexAttr],
                         val primAttrs: TreeMap[String, PrimitiveAttr]) {
  //------------------------------------------------------------------------------------------------
  //   I N T E R N A L   C L A S S E S 
  //------------------------------------------------------------------------------------------------

  private trait AttrValue {
    def write(out: Writer)
  }

  private class IntAttrValue(val value: Int)
    extends AttrValue {
    def write(out: Writer) { out.write(value.toString) }
  }

  private class Index2iAttrValue(val value: Index2i)
    extends AttrValue {
    def write(out: Writer) { out.write(value.x + " " + value.y) }
  }

  private class Index3iAttrValue(val value: Index3i)
    extends AttrValue {
    def write(out: Writer) { out.write(value.x + " " + value.y + " " + value.z) }
  }

  private class FloatAttrValue(val value: Double)
    extends AttrValue {
    def write(out: Writer) { out.write("%.6f".format(value)) }
  }

  private class Vec2dAttrValue(val value: Vec2d)
    extends AttrValue {
    def write(out: Writer) { out.write("%.6f %.6f".format(value.x, value.y)) }
  }

  private class Vec3dAttrValue(val value: Vec3d)
    extends AttrValue {
    def write(out: Writer) { out.write("%.6f %.6f %.6f".format(value.x, value.y, value.z)) }
  }

  private class StringAttrValue(val value: Int)
    extends AttrValue {
    def write(out: Writer) { out.write(value) }
  }

  //------------------------------------------------------------------------------------------------
  //   H E A D E R  /  F O O T E R
  //------------------------------------------------------------------------------------------------

  /** Write the GEO header. */
  def writeHeader(out: Writer) {
    val noAttrs = (pointAttrs.isEmpty && vertexAttrs.isEmpty && primAttrs.isEmpty)
    out.write(
      "PGEOMETRY V5\n" +
      "NPoints " + numPoints + " NPrims " + numPrims + "\n" +
      "NPointGroups " + pointGroups.size + " NPrimGroups " + primGroups.size + "\n" +
      "NPointAttrib " + pointAttrs.size + " NVertexAttrib " + vertexAttrs.size + " " +
      "NPrimAttrib " + primAttrs.size + " NAttrib " + (if(noAttrs) "0" else "1") + "\n")
  }

  /** Write the GEO footer. */
  def writeFooter(out: Writer) {
    val attrNames = new TreeSet[String] ++ pointAttrs.keySet ++ vertexAttrs.keySet ++ primAttrs.keySet
    if (!attrNames.isEmpty) {
      out.write("DetailAttrib\n" +
        "varmap 1 index " + attrNames.size + " ")
      for (a <- attrNames)
        out.write("\"" + a + " -> " + a.toUpperCase + "\" ")
      out.write("\n")

      out.write(" (0)\n") // no idea why!
    }

    for ((name, indices) <- pointGroups) {
      out.write(name + " unordered\n" +
        numPoints + " ")
      for (i <- 0 until numPoints)
        out.write(if (indices.contains(i)) "1" else "0")
      out.write("\n")
    }

    for ((name, indices) <- primGroups) {
      out.write(name + " unordered\n" +
        numPrims + " ")
      for (i <- 0 until numPrims)
        out.write(if (indices.contains(i)) "1" else "0")
      out.write("\n")
    }

    out.write("beginExtra\n" +
      "endExtra\n")
  }

  //------------------------------------------------------------------------------------------------
  //   P O I N T S
  //------------------------------------------------------------------------------------------------

  /** Write the Point Attribute dictionary. */
  def writePointAttrs(out: Writer) {
    out.write("PointAttrib\n")
    for (attr <- pointAttrs.values)
      attr.write(out)
  }

  /** Temporary table of point attribute values to apply to the next emitted point. */
  private var pointValues = new TreeMap[String, AttrValue]

  /** Set the point attribute for the next emitted point. */
  def setPointAttr(name: String, value: Int) {
    if (!pointAttrs.contains(name))
      throw new IllegalArgumentException("No point attribute name (" + name + ") is defined!")
    if (!pointAttrs(name).isInstanceOf[PointIntAttr])
      throw new IllegalArgumentException("Wrong type for attribute!")
    pointValues = pointValues + (name -> new IntAttrValue(value))
  }

  /** Set the point attribute for the next emitted point. */
  def setPointAttr(name: String, value: Double) {
    if (!pointAttrs.contains(name))
      throw new IllegalArgumentException("No point attribute name (" + name + ") is defined!")
    if (!pointAttrs(name).isInstanceOf[PointFloatAttr])
      throw new IllegalArgumentException("Wrong type for attribute!")
    pointValues = pointValues + (name -> new FloatAttrValue(value))
  }

  // ... 

  /** Write a Point along with any previously set Point Attributes. */
  def writePoint(out: Writer, p: Pos3d) {
    out.write("%.6f %.6f %.6f 1".format(p.x, p.y, p.z))
    if (!pointAttrs.isEmpty) {
      out.write(" (")
      for (aname <- pointAttrs.keys) {
        if (pointValues.contains(aname)) pointValues(aname).write(out)
        else pointAttrs(aname).writeUndefined(out)
        out.write(" ")
      }
      out.write(")")
    }
    out.write("\n")

    pointValues = new TreeMap[String, AttrValue]
  }

  //------------------------------------------------------------------------------------------------
  //   P R I M I T I V E S
  //------------------------------------------------------------------------------------------------

  /** Write the Primitive Attribute dictionary. */
  def writePrimAttrs(out: Writer) {
    out.write("PrimitiveAttrib\n")
    for (attr <- primAttrs.values)
      attr.write(out)
  }

  /** Write the collected Primitive attribute Values. */
  private def writePrimAttrValues(out: Writer) {
    if (!primAttrs.isEmpty) {
      out.write(" [ ")
      for (aname <- primAttrs.keys) {
        if (primValues.contains(aname)) primValues(aname).write(out)
        else primAttrs(aname).writeUndefined(out)
        out.write(" ")
      }
      out.write("]")
    }
    
    primValues = new TreeMap[String, AttrValue]    
  }
  
  /** Temporary table of primitive attribute values to apply to the next emitted primitive. */
  private var primValues = new TreeMap[String, AttrValue]

  /** Set the primitive attribute for the next emitted primitive. */
  def setPrimAttr(name: String, value: Int) {
    if (!primAttrs.contains(name))
      throw new IllegalArgumentException("No primitive attribute name (" + name + ") is defined!")
    if (!primAttrs(name).isInstanceOf[PrimitiveIntAttr])
      throw new IllegalArgumentException("Wrong type for attribute!")
    primValues = primValues + (name -> new IntAttrValue(value))
  }

  /** Set the primitive attribute for the next emitted primitive. */
  def setPrimAttr(name: String, value: Double) {
    if (!primAttrs.contains(name))
      throw new IllegalArgumentException("No primitive attribute name (" + name + ") is defined!")
    if (!primAttrs(name).isInstanceOf[PrimitiveFloatAttr])
      throw new IllegalArgumentException("Wrong type for attribute!")
    primValues = primValues + (name -> new FloatAttrValue(value))
  }

  // ... 

  /** Write the header for a Polygon primitive. */
  def writePolygon(out: Writer, numFaces: Int) {
    out.write("Run " + numFaces + " Poly\n")
  }

  /** Write a triangular Polygon face along with any previously set Primitive Attributes. */
  def writeTriangle(out: Writer, i: Index3i) {
    if (i.forany(e => e < 0 || e >= numPoints))
      throw new IllegalArgumentException("Index out of bounds!")
    out.write(" 3 < " + i.x + " " + i.y + " " + i.z)
    writePrimAttrValues(out)
    out.write("\n")
  }

  /** Write a polygonal line. */
  def writePolyLine(out: Writer, idx: List[Int]) {
    out.write("Poly " + idx.size + " : " + idx.map(_.toString).reduce(_ + " " + _) + " \n")
    writePrimAttrValues(out)
    out.write("\n")
  }

}

/** GeoWriter factory object. */
object GeoWriter {
  /** Create a new Houdini GEO file writer.
    * @param numPoint The total number of points.
    * @param numPrims The total number of primitives.
    * @param pointGroups The names and member indices of the point groups.
    * @param primGroups The names and member indices of the primitive groups.
    * @param pointAttrs The point attribute definitions.
    * @param vertexAttrs The vertex attribute definitions.
    * @param primAttrs The primitive attribute definitions.
    */
  def apply(numPoints: Int, numPrims: Int,
            pointGroups: Map[String, Set[Int]] = TreeMap(),
            primGroups: Map[String, Set[Int]] = TreeMap(),
            pointAttrs: Seq[PointAttr] = List(),
            vertexAttrs: Seq[VertexAttr] = List(),
            primAttrs: Seq[PrimitiveAttr] = List()) =
    new GeoWriter(numPoints, numPrims,
      new TreeMap[String, Set[Int]] ++ pointGroups, new TreeMap[String, Set[Int]] ++ primGroups,
      (new TreeMap[String, PointAttr] /: pointAttrs) { case (rtn, a) => rtn + (a.name -> a) },
      (new TreeMap[String, VertexAttr] /: vertexAttrs) { case (rtn, a) => rtn + (a.name -> a) },
      (new TreeMap[String, PrimitiveAttr] /: primAttrs) { case (rtn, a) => rtn + (a.name -> a) })
}