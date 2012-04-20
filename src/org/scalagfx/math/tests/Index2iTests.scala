// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math.tests

import org.scalagfx.math.{Index2i,Scalar}

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.{Arbitrary,Gen}

object Index2iTests
{
  import ScalarTests._

  val genIndex2iNZ =   
    for {
      x <- genIntNZ
      y <- genIntNZ
    } yield Index2i(x.toInt, y.toInt)

  val genIndex2i =   
    for {
      x <- genInt
      y <- genInt
    } yield Index2i(x.toInt, y.toInt) 

  implicit def arbIndex2i: Arbitrary[Index2i] = Arbitrary(genIndex2i)
}

@RunWith(classOf[JUnitRunner])
class Index2iTests extends PropSpec with PropertyChecks 
{
  import ScalarTests._
  import Index2iTests._
  import Index2i._
  
  // Creation.
  property("construct") {
    forAll (genInt, genInt) {
      (x: Int, y: Int) => {
        val v = Index2i(x, y) 
        assert(x == v.x)
        assert(y == v.y) 
      }
    }
  }

  property("min") {
    forAll { 
      (a: Index2i, b: Index2i) => {
        val r = min(a, b) 
        assert(r.x <= a.x)
        assert(r.x <= b.x) 
        assert(r.y <= a.y)
        assert(r.y <= b.y)
      }
    }
  }

  property("max") {
    forAll { 
      (a: Index2i, b: Index2i) => {
        val r = max(a, b) 
        assert(r.x >= a.x)
        assert(r.x >= b.x)  
        assert(r.y >= a.y)
        assert(r.y >= b.y)
      }
    }
  }

  // Utility.
  property("compwise") { 
    forAll { (a: Index2i, b: Index2i) => assert(compwise(a, b, _ + _) == (a + b)) }
  }

  // Component Ops.
  property("new-comp") {
    forAll { 
      (a: Index2i, b: Index2i) => { 
        assert(a.newX(b.x).newY(b.y) == b) 
        assert(a.newX(b.x) == a.newComp(0, b.x))  
        assert(a.newY(b.y) == a.newComp(1, b.y))
      }
    }
  }

  // Unary Ops.
  property("negated") { 
    forAll { (a: Index2i) => assert(a.negated.negated == a) }
  }

  property("minComp") {
    forAll { (v: Index2i) => assert(Scalar.equiv(math.min(v.x, v.y), v.minComp)) }
  }

  property("maxComp") { 
    forAll { (v: Index2i) => assert(Scalar.equiv(math.max(v.x, v.y), v.maxComp)) } 
  }
  
  property("abs") { 
    forAll { 
      (v: Index2i) => {
        val av = v.abs
        assert(Scalar.equiv(math.abs(v.x), av.x)) 
        assert(Scalar.equiv(math.abs(v.y), av.y))
      }
    }
  }

  // Scalar Ops.
  property("scalar-add") {
    forAll (genIndex2i, genInt) { 
      (v: Index2i, s: Int) => {
        val r = v + s
        assert(Scalar.equiv(v.x+s, r.x))
        assert(Scalar.equiv(v.y+s, r.y))
      }
    }
  }

  property("scalar-subtract") { 
    forAll (genIndex2i, genInt) { 
      (v: Index2i, s: Int) => {
        val r = v - s
        assert(Scalar.equiv(v.x-s, r.x))
        assert(Scalar.equiv(v.y-s, r.y))
      }
    }
  }

  property("scalar-multiply") { 
    forAll (genIndex2i, genInt) { 
      (v: Index2i, s: Int) => {
        val r = v * s
        assert(Scalar.equiv(v.x*s, r.x))
        assert(Scalar.equiv(v.y*s, r.y))
      }
    }
  }
  
  // Operators.
  property("add-create") {
    forAll { (_: Unit) => assert((zero + one) == unitX+unitY) }
  }

  property("add-associativity") { 
    forAll { 
      (a: Index2i, b: Index2i, c: Index2i) => 
        assert(((a + b) + c) == (a + (b + c)))
    }
  }

  property("add-commutativity") { 
    forAll { 	
      (a: Index2i, b: Index2i, c: Index2i) => {
        val r = a + b + c
        assert(r == (b + c + a))
        assert(r == (c + a + b))
        assert(r == (c + b + a))
      }
    }
  }
  
  property("multiply-associativity") {
    forAll { 
      (a: Index2i, b: Index2i, c: Index2i) => 
        assert(((a * b) * c) == (a * (b * c)))
    }
  }

  property("multiply-commutativity") { 
    forAll { 
      (a: Index2i, b: Index2i, c: Index2i) => {
        val r = a * b * c
        assert(r == (b * c * a))
        assert(r == (c * a * b))
        assert(r == (c * b * a))
      }
    }
  }
  
  property("subtract") {
    forAll { (a: Index2i, b: Index2i) => assert((a - b) == (a + b.negated)) } 
  }
  
  property("distributivity") {
    forAll { 
      (a: Index2i, b: Index2i, c: Index2i) => 
        assert((a * (b + c)) == ((a * b) + (a * c)))
        assert((a * (b - c)) == ((a * b) - (a * c)))
    }
  }
     
  property("dot-product-unit") { 
      assert((unitX dot unitY) == 0) 
      assert((unitY dot unitX) == 0)	
  }
  
  // Comparison.
  property("equals") {
    forAll { (v: Index2i) => assert(v == v) }
  }

  property("not-equals") { 
    forAll { (v: Index2i) => assert(v != (v + one)) }
  }
  
  property("hash-code") { 
    forAll { 
      (a: Index2i, b: Index2i) => 
        if(a.hashCode == b.hashCode) assert(a == b) else assert(a != b)
    }
  }

  property("anyLt-allGte") {
    forAll { 
      (a: Index2i, b: Index2i) => 
        if(a anyLt b) assert(!(a allGte b)) else assert(a allGte b)
    }
  }
            
  property("anyLte-allGt") {
    forAll { 
      (a: Index2i, b: Index2i) => 
        if(a anyLte b) assert(!(a allGt b)) else assert(a allGt b) 
    }
  }
    
  property("anyGt-allLte") { 
    forAll { 
      (a: Index2i, b: Index2i) => 
        if(a anyGt b) assert(!(a allLte b)) else assert(a allLte b)
    }
  }

  property("anyGte-allLt") { 
    forAll { 
      (a: Index2i, b: Index2i) => 
        if(a anyGte b) assert(!(a allLt b)) else assert(a allLt b) 
    }
  }

  // Utility.
  property("apply") {
    forAll { 
      (v: Index2i) => 
        assert(v(0) == v.x)
        assert(v(1) == v.y)
    }
  }
  
  property("forall") { 
    forAll { 
      (v: Index2i) => 
        assert(v.forall(_ < 0) == ((v.x < 0) && (v.y < 0)))
    }
  }

  property("forall-dual") { 
    forAll { 
      (a: Index2i, b: Index2i) => 
        assert(a.forall(b)(_ < _) == ((a.x < b.x) && (a.y < b.y)))
    }
  }

  property("forany") { 
    forAll { 
      (v: Index2i) => 
        assert(v.forany(_ < 0) == ((v.x < 0) || (v.y < 0)))
    }
  }

  property("forany-dual") {
    forAll { 
      (a: Index2i, b: Index2i) => 
        assert(a.forany(b)(_ < _) == ((a.x < b.x) || (a.y < b.y)))
    }
  }

  property("map") { 
    forAll { (v: Index2i) => assert(v.map(_ * 2) == (v * 2)) }
  }

  property("foldLeft") { 
    forAll (genIndex2i, genInt) { 
      (v: Index2i, d: Int) => assert(v.foldLeft(d)(_ + _) == (d + v.x + v.y)) 
    }
  }

  property("foldRight") { 
    forAll (genIndex2i, genInt) {
      (v: Index2i, d: Int) => assert(v.foldRight(d)(_ + _) == (d + v.y + v.x)) 
    }
  }

  // Conversion.
  property("toVector4d") { 
    forAll { 
      (a: Index2i) => {
        val b = a.toVector4d
        assert(a.x.toDouble == b.x)
        assert(a.y.toDouble == b.y)
        assert(0.0 == b.z)
        assert(1.0 == b.w) 
      }
    }
  }

  property("toVector3d") { 
    forAll { 
      (a: Index2i) => {
        val b = a.toVector3d
        assert(a.x.toDouble == b.x)
        assert(a.y.toDouble == b.y)
        assert(0.0 == b.z)
      }
    }
  }

  property("toVector2d") {
    forAll { 
      (a: Index2i) => {
        val b = a.toVector2d
        assert(a.x.toDouble == b.x)
        assert(a.y.toDouble == b.y)
      }
    }
  }

  property("toVec3d") { 
    forAll { 
      (a: Index2i) => {
        val b = a.toVec3d
        assert(a.x.toDouble == b.x)
        assert(a.y.toDouble == b.y)
        assert(0.0 == b.z)
      }
    }
  }

  property("toVec2d") {
    forAll { 
      (a: Index2i) => {
        val b = a.toVec2d
        assert(a.x.toDouble == b.x)
        assert(a.y.toDouble == b.y)
      }
    }
  }

  property("toPos3d") { 
    forAll { 
      (a: Index2i) => {
        val b = a.toPos3d
        assert(a.x.toDouble == b.x)
        assert(a.y.toDouble == b.y)
        assert(0.0 == b.z)
      }
    }
  }

  property("toPos2d") { 
    forAll { 
      (a: Index2i) => {
        val b = a.toPos2d
        assert(a.x.toDouble == b.x)
        assert(a.y.toDouble == b.y)
      }
    }
  }
      
  property("toIndex3i") { 
    forAll { 
      (a: Index2i) => {
        val b = a.toIndex3i
        assert(a.x == b.x)
        assert(a.y == b.y)
        assert(0 == b.z)
      }
    }
  }
      
  property("toIndex2i") { 
    forAll { 
      (a: Index2i) => {
        val b = a.toIndex2i
        assert(a.x == b.x) 
        assert(a.y == b.y)
      }
    }
  }

  property("toNative") { 
    forAll { 
      (a: Index2i) => {
        val buf = a.toNative
        assert(buf.capacity == 2) 
        assert(buf.get == a.x)
        assert(buf.get == a.y)
      }
    }
  }
    
  property("toNativeShorts") { 
    forAll { 
      (a: Index2i) => {
        val buf = a.toNativeShorts
        assert(buf.capacity == 2) 
        assert(buf.get == a.x.toShort)
        assert(buf.get == a.y.toShort)
      }
    }
  }
}

