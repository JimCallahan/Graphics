// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math.tests

import org.scalagfx.math.{Index3i,Scalar}

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.{Arbitrary,Gen}

object Index3iTests
{
  import ScalarTests._
  
  val genIndex3iNZ =   
    for {
      x <- genIntNZ
      y <- genIntNZ
      z <- genIntNZ
    } yield Index3i(x.toInt, y.toInt, z.toInt) 

  val genIndex3i =   
    for {
      x <- genInt
      y <- genInt
      z <- genInt
    } yield Index3i(x.toInt, y.toInt, z.toInt) 

  implicit def arbIndex3i: Arbitrary[Index3i] = Arbitrary(genIndex3i)
}

@RunWith(classOf[JUnitRunner])
class Index3iTests extends PropSpec with PropertyChecks 
{
  import ScalarTests._
  import Index3iTests._
  import Index3i._
  
  // Creation.
  property("construct") {
    forAll (genInt, genInt, genInt) {
      (x: Int, y: Int, z: Int) => {
        val v = Index3i(x, y, z) 
        assert(x == v.x)
        assert(y == v.y)
        assert(z == v.z)
      }
    }
  }

  property("min") { 
    forAll { 
      (a: Index3i, b: Index3i) => {
        val r = min(a, b) 
        assert(r.x <= a.x)
        assert(r.x <= b.x)
        assert(r.y <= a.y)
        assert(r.y <= b.y)
        assert(r.z <= a.z)
        assert(r.z <= b.z)
      }
    }
  }

  property("max") {
    forAll { 
      (a: Index3i, b: Index3i) => {
        val r = max(a, b) 
        assert(r.x >= a.x)
        assert(r.x >= b.x)
        assert(r.y >= a.y)
        assert(r.y >= b.y)
        assert(r.z >= a.z)
        assert(r.z >= b.z)
      }
    }
  }

  // Utility.
  property("compwise") {
    forAll { (a: Index3i, b: Index3i) => assert(compwise(a, b, _ + _) == (a + b)) }
  }

  // Component Ops.
  property("new-comp") {
    forAll { 
      (a: Index3i, b: Index3i) => 
        assert(a.newX(b.x).newY(b.y).newZ(b.z) == b)
        assert(a.newX(b.x) == a.newComp(0, b.x)) 
        assert(a.newY(b.y) == a.newComp(1, b.y)) 
        assert(a.newZ(b.z) == a.newComp(2, b.z))
    }
  }

  // Unary Ops.
  property("negated") {
    forAll { (a: Index3i) => assert(a.negated.negated == a) }
  }

  property("minComp") {
    forAll { 
      (v: Index3i) => 
        assert(Scalar.equiv(math.min(math.min(v.x, v.y), v.z), v.minComp)) 
    }
  }

  property("maxComp") { 
    forAll { 
      (v: Index3i) => 
        assert(Scalar.equiv(math.max(math.max(v.x, v.y), v.z), v.maxComp)) 
    }
  }
  
  property("abs") { 
    forAll { 
      (v: Index3i) => {
        val av = v.abs
        assert(Scalar.equiv(math.abs(v.x), av.x)) 
        assert(Scalar.equiv(math.abs(v.y), av.y)) 
        assert(Scalar.equiv(math.abs(v.z), av.z))
      }
    }
  }

  // Scalar Ops.
  property("scalar-add") {
    forAll (genIndex3i, genInt) { 
      (v: Index3i, s: Int) => {
        val r = v + s
        assert(Scalar.equiv(v.x+s, r.x))
        assert(Scalar.equiv(v.y+s, r.y))
        assert(Scalar.equiv(v.z+s, r.z))
      }
    }
  }

  property("scalar-subtract") { 
    forAll (genIndex3i, genInt) { 
      (v: Index3i, s: Int) => {
        val r = v - s
        assert(Scalar.equiv(v.x-s, r.x))
        assert(Scalar.equiv(v.y-s, r.y))
        assert(Scalar.equiv(v.z-s, r.z))
      }
    }
  }

  property("scalar-multiply") {
    forAll (genIndex3i, genInt) { 
      (v: Index3i, s: Int) => {
        val r = v * s
        assert(Scalar.equiv(v.x*s, r.x))
        assert(Scalar.equiv(v.y*s, r.y))
        assert(Scalar.equiv(v.z*s, r.z))
      }
    }
  }
  
  // Operators.
  property("add-create") {
    forAll { (_: Unit) => assert((zero + one) == unitX+unitY+unitZ) }
  }

  property("add-associativity") { 
    forAll { 
      (a: Index3i, b: Index3i, c: Index3i) => 
        assert(((a + b) + c) == (a + (b + c)))
    }
  }

  property("add-commutativity") {
    forAll { 
      (a: Index3i, b: Index3i, c: Index3i) => {
        val r = a + b + c
        assert(r == (b + c + a))
        assert(r == (c + a + b))
        assert(r == (c + b + a))
      }
    }
  }
       
  property("multiply-associativity") {
    forAll { 
      (a: Index3i, b: Index3i, c: Index3i) => 
        assert(((a * b) * c) == (a * (b * c))) 
    }
  }
  
  property("multiply-commutativity") { 
    forAll { 
      (a: Index3i, b: Index3i, c: Index3i) => {
        val r = a * b * c
        assert(r == (b * c * a))
        assert(r == (c * a * b))
        assert(r == (c * b * a))
      }
    }
  }
  
  property("subtract") { 
    forAll { (a: Index3i, b: Index3i) => assert((a - b) == (a + b.negated)) }
  }
  
  property("distributivity") {
    forAll { 
      (a: Index3i, b: Index3i, c: Index3i) => 
        assert((a * (b + c)) == ((a * b) + (a * c)))
        assert((a * (b - c)) == ((a * b) - (a * c)))
    }
  }

  property("dot-product-unit") {
    forAll { 
      (_: Unit) => 
        assert((unitX dot unitY) == 0)
        assert((unitY dot unitZ) == 0)
        assert((unitZ dot unitX) == 0)
    }
  }
  
  property("cross-product-unit") { 
    forAll { 
      (_: Unit) => 
        assert((unitX cross unitY) == unitZ)
        assert((unitY cross unitZ) == unitX)
        assert((unitZ cross unitX) == unitY)
    }
  }

  // Comparison.
  property("equals") {
    forAll { (v: Index3i) => assert(v == v) }
  }

  property("not-equals") {
    forAll { (v: Index3i) => assert(v != (v + one)) }
  }
  
  property("hash-code") { 
    forAll { 
      (a: Index3i, b: Index3i) => {
        if(a.hashCode == b.hashCode) assert(a == b) else assert(a != b)
      }
    }
  }
       
  property("anyLt-allGte") { 
    forAll { 
      (a: Index3i, b: Index3i) => 
        if(a anyLt b) assert(!(a allGte b)) else assert(a allGte b) 
    }
  }
    
  property("anyLte-allGt") {
    forAll { 
      (a: Index3i, b: Index3i) => 
        if(a anyLte b) assert(!(a allGt b)) else assert(a allGt b)
    }
  }
    
  property("anyGt-allLte") { 
    forAll { 
      (a: Index3i, b: Index3i) => 
        if(a anyGt b) assert(!(a allLte b)) else assert(a allLte b) 
    }
  }
  
  property("anyGte-allLt") {
    forAll { 
      (a: Index3i, b: Index3i) => 
        if(a anyGte b) assert(!(a allLt b)) else assert(a allLt b)
    }
  }

  // Utility.
  property("apply") {
    forAll { 
      (v: Index3i) => 
        assert(v(0) == v.x)
        assert(v(1) == v.y)
        assert(v(2) == v.z) 
    }
  }
  
  property("forall") {
    forAll { 
      (v: Index3i) => 
        assert(v.forall(_ < 0) == ((v.x < 0) && (v.y < 0) && (v.z < 0)))
    }
  }

  property("forall-dual") { 
    forAll { 
      (a: Index3i, b: Index3i) => 
        assert(a.forall(b)(_ < _) == ((a.x < b.x) && (a.y < b.y) && (a.z < b.z)))
    }
  }
  
  property("forany") {
    forAll { 
      (v: Index3i) => 
        assert(v.forany(_ < 0) == ((v.x < 0) || (v.y < 0) || (v.z < 0)))
    }
  }

  property("forany-dual") {
    forAll { 
      (a: Index3i, b: Index3i) => 
        assert(a.forany(b)(_ < _) == ((a.x < b.x) || (a.y < b.y) || (a.z < b.z)))
    }
  }

  property("map") {
    forAll { (v: Index3i) => assert(v.map(_ * 2) == (v * 2)) }
  }

  property("foldLeft") { 
    forAll (genIndex3i, genInt) { 
      (v: Index3i, d: Int) => assert(v.foldLeft(d)(_ + _) == (d + v.x + v.y + v.z)) 
    }
  }

  property("foldRight") { 
    forAll (genIndex3i, genInt) {
      (v: Index3i, d: Int) => assert(v.foldRight(d)(_ + _) == (d + v.z + v.y + v.x)) 
    }
  }

  // Conversion.
  property("toVector4d") {
    forAll { 
      (a: Index3i) => {
        val b = a.toVector4d
        assert(a.x.toDouble == b.x)
        assert(a.y.toDouble == b.y)
        assert(a.z.toDouble == b.z)
        assert(1.0 == b.w) 
      }
    }
  }

  property("toVector3d") { 
    forAll { 
      (a: Index3i) => {
        val b = a.toVector3d
        assert(a.x.toDouble == b.x)
        assert(a.y.toDouble == b.y)
        assert(a.z.toDouble == b.z)
      }
    }
  }

  property("toVector2d") { 
    forAll { 
      (a: Index3i) => {
        val b = a.toVector2d
        assert(a.x.toDouble == b.x)
        assert(a.y.toDouble == b.y)
      }
    }
  }
  
  property("toVec3d") { 
    forAll { 
      (a: Index3i) => {
        val b = a.toIndex3i
        assert(a.x.toDouble == b.x)
        assert(a.y.toDouble == b.y)
        assert(a.z.toDouble == b.z)
      }
    }
  }
  
  property("toVec2d") { 
    forAll { 
      (a: Index3i) => {
        val b = a.toVec2d
        assert(a.x.toDouble == b.x)
        assert(a.y.toDouble == b.y)
      }
    }
  }

  property("toPos3d") { 
    forAll { 
      (a: Index3i) => {
        val b = a.toPos3d
        assert(a.x.toDouble == b.x)
        assert(a.y.toDouble == b.y)
        assert(a.z.toDouble == b.z)
      }
    }
  }

  property("toPos2d") { 
    forAll { 
      (a: Index3i) => {
        val b = a.toPos2d
        assert(a.x.toDouble == b.x)
        assert(a.y.toDouble == b.y)
      }
    }
  }
      
  property("toIndex3i") { 
    forAll { 
      (a: Index3i) => {
        val b = a.toIndex3i
        assert(a.x == b.x)
        assert(a.y == b.y)
        assert(a.z == b.z)
      }
    }
  }
      
  property("toIndex2i") { 
    forAll { 
      (a: Index3i) => {
        val b = a.toIndex2i
        assert(a.x == b.x)
        assert(a.y == b.y)
      }
    }
  }

  property("toNative") { 
    forAll { 
      (a: Index3i) => {
        val buf = a.toNative
        assert(buf.capacity == 3)
        assert(buf.get == a.x)
        assert(buf.get == a.y)
        assert(buf.get == a.z)
      }
    }
  }

  property("toNativeShorts") { 
    forAll { 
      (a: Index3i) => {
        val buf = a.toNativeShorts
        assert(buf.capacity == 3)
        assert(buf.get == a.x.toShort)
        assert(buf.get == a.y.toShort)
        assert(buf.get == a.z.toShort)
      }
    }
  }
}