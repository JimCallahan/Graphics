package org.scalagfx.math.tests

import org.scalagfx.math.Scalar._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Gen

object ScalarTests 
{
  val genBool = 
    Gen.oneOf(Gen.value(true), Gen.value(false)) 

  val genInt = 
     Gen.oneOf(Gen.choose(-10000, -1000), 
               Gen.choose(-1000,  -100), 
               Gen.choose(-100,   -10), 
               Gen.choose(-10,    -1),  
               Gen.value(0),   
               Gen.choose(1,    10),  
               Gen.choose(10,   100),  
               Gen.choose(100,  1000),  
               Gen.choose(1000, 10000))

  val genIntNZ = 
     Gen.oneOf(Gen.choose(-10000,  -1000), 
               Gen.choose(-1000,  -100), 
               Gen.choose(-100,  -10), 
               Gen.choose(-10,  -1), 
               Gen.choose(1,    10),  
               Gen.choose(10,  100),  
               Gen.choose(100,  1000),  
               Gen.choose(1000,  10000))

  val genIntNorm = 
    Gen.choose(0, 100)

  val genFloat = 
     Gen.oneOf(Gen.choose(-1.0E4f,  -1.0E3f), 
               Gen.choose(-1.0E3f,  -1.0E2f), 
               Gen.choose(-1.0E2f,  -1.0E1f), 
               Gen.choose(-1.0E1f,  -1.0f), 
               Gen.choose(-1.0f,    -1.0E-1f), 
               Gen.choose(-1.0E-1f, -1.0E-2f),  
               Gen.choose(-1.0E-2f, -1.0E-3f),  
               Gen.value(0.0f),   
               Gen.choose(1.0E-3f, 1.0E-2f), 
               Gen.choose(1.0E-2f, 1.0E-1f), 
               Gen.choose(1.0E-1f, 1.0f),    
               Gen.choose(1.0f,    1.0E1f),  
               Gen.choose(1.0E1f,  1.0E2f),  
               Gen.choose(1.0E2f,  1.0E3f),  
               Gen.choose(1.0E3f,  1.0E4f))

  val genFloatNZ = 
     Gen.oneOf(Gen.choose(-1.0E4f,  -1.0E3f), 
               Gen.choose(-1.0E3f,  -1.0E2f), 
               Gen.choose(-1.0E2f,  -1.0E1f), 
               Gen.choose(-1.0E1f,  -1.0f), 
               Gen.choose(-1.0f,    -1.0E-1f), 
               Gen.choose(-1.0E-1f, -1.0E-2f),  
               Gen.choose(-1.0E-2f, -1.0E-3f),  
               Gen.choose(1.0E-3f, 1.0E-2f), 
               Gen.choose(1.0E-2f, 1.0E-1f), 
               Gen.choose(1.0E-1f, 1.0f),    
               Gen.choose(1.0f,    1.0E1f),  
               Gen.choose(1.0E1f,  1.0E2f),  
               Gen.choose(1.0E2f,  1.0E3f),  
               Gen.choose(1.0E3f,  1.0E4f))

  val genFloatSmall = 
     Gen.oneOf(Gen.choose(-1.0E1f,  -1.0f), 
               Gen.choose(-1.0f,    -1.0E-1f), 
               Gen.value(0.0f),   
               Gen.choose(1.0E-1f, 1.0f),    
               Gen.choose(1.0f,    1.0E1f))

  val genFloatNorm = 
    Gen.choose(0.0f, 1.0f)

  val genDouble = 
     Gen.oneOf(Gen.choose(-1.0E7,  -1.0E6), 
               Gen.choose(-1.0E6,  -1.0E5), 
               Gen.choose(-1.0E5,  -1.0E4), 
               Gen.choose(-1.0E4,  -1.0E3), 
               Gen.choose(-1.0E3,  -1.0E2), 
               Gen.choose(-1.0E2,  -1.0E1), 
               Gen.choose(-1.0E1,  -1.0), 
               Gen.choose(-1.0,    -1.0E-1), 
               Gen.choose(-1.0E-1, -1.0E-2),  
               Gen.choose(-1.0E-2, -1.0E-3),  
               Gen.choose(-1.0E-3, -1.0E-4),  
               Gen.value(0.0),  
               Gen.choose(1.0E-4, 1.0E-3),   
               Gen.choose(1.0E-3, 1.0E-2), 
               Gen.choose(1.0E-2, 1.0E-1), 
               Gen.choose(1.0E-1, 1.0),    
               Gen.choose(1.0,    1.0E1),  
               Gen.choose(1.0E1,  1.0E2),  
               Gen.choose(1.0E2,  1.0E3),  
               Gen.choose(1.0E3,  1.0E4),  
               Gen.choose(1.0E4,  1.0E5),  
               Gen.choose(1.0E5,  1.0E6),  
               Gen.choose(1.0E6,  1.0E7))

  val genDoubleNZ = 
     Gen.oneOf(Gen.choose(-1.0E7,  -1.0E6), 
               Gen.choose(-1.0E6,  -1.0E5), 
               Gen.choose(-1.0E5,  -1.0E4), 
               Gen.choose(-1.0E4,  -1.0E3), 
               Gen.choose(-1.0E3,  -1.0E2), 
               Gen.choose(-1.0E2,  -1.0E1), 
               Gen.choose(-1.0E1,  -1.0), 
               Gen.choose(-1.0,    -1.0E-1), 
               Gen.choose(-1.0E-1, -1.0E-2),  
               Gen.choose(-1.0E-2, -1.0E-3),  
               Gen.choose(-1.0E-3, -1.0E-4), 
               Gen.choose(1.0E-4, 1.0E-3),   
               Gen.choose(1.0E-3, 1.0E-2), 
               Gen.choose(1.0E-2, 1.0E-1), 
               Gen.choose(1.0E-1, 1.0),    
               Gen.choose(1.0,    1.0E1),  
               Gen.choose(1.0E1,  1.0E2),  
               Gen.choose(1.0E2,  1.0E3),  
               Gen.choose(1.0E3,  1.0E4),  
               Gen.choose(1.0E4,  1.0E5),  
               Gen.choose(1.0E5,  1.0E6),  
               Gen.choose(1.0E6,  1.0E7))

  val genDoubleSmall = 
     Gen.oneOf(Gen.choose(-1.0E3,  -1.0E2), 
               Gen.choose(-1.0E2,  -1.0E1), 
               Gen.choose(-1.0E1,  -1.0), 
               Gen.choose(-1.0,    -1.0E-1), 
               Gen.choose(-1.0E-1, -1.0E-2),  
               Gen.choose(-1.0E-2, -1.0E-3),  
               Gen.value(0.0),   
               Gen.choose(1.0E-3, 1.0E-2), 
               Gen.choose(1.0E-2, 1.0E-1), 
               Gen.choose(1.0E-1, 1.0),    
               Gen.choose(1.0,    1.0E1),  
               Gen.choose(1.0E1,  1.0E2),  
               Gen.choose(1.0E2,  1.0E3))

  val genDoubleMed = 
     Gen.oneOf(Gen.choose(-1.0E3,  -1.0E2), 
               Gen.choose(-1.0E2,  -1.0E1), 
               Gen.choose(-1.0E1,  -1.0), 
               Gen.choose(1.0,    1.0E1),  
               Gen.choose(1.0E1,  1.0E2),  
               Gen.choose(1.0E2,  1.0E3))

  val genDoubleNorm = 
    Gen.choose(0.0, 1.0)
}

@RunWith(classOf[JUnitRunner])
class ScalarTests extends PropSpec with PropertyChecks
{
  import ScalarTests._
  import scala.math.abs
  
  // Whether the two values are within a given epsilon of each other.
  {
    import java.lang.Short._
    val gv = Gen.choose(MIN_VALUE/2, MAX_VALUE/2)
    val ge = Gen.choose(0, 128)
    property("equiv-ep-short") { 
      forAll (gv, ge) { 
        (v: Int, e: Int) => {
          val (sv, se) = (v.toShort, e.toShort) 
          assert(equiv(sv, sv+se, abs(se)+1))
          assert(equiv(sv, sv-se, abs(se)+1))
        } 
      }
    }
  }
  
  {
    val gv = Gen.choose(-2147483649L, 2147483649L)
    val ge = Gen.choose(0L, 16384L)
    property("equiv-ep-long") { 
      forAll (gv, ge) { 
        (v: Long, e: Long) => {
          assert(equiv(v, v+e, abs(e)+1))
          assert(equiv(v, v-e, abs(e)+1))
        }
      }
    }
  }   

  property("equiv-ep-float") { 
    forAll (genFloat, Gen.choose(0.0f, 1.0f)) { 
      (v: Float, e: Float) => {
        val (fv, fe) = (v, e) 
        assert(equiv(fv, fv+fe, abs(fe*2.0f))) 
      }
    }
  }
  
  property("equiv-ep-double") { 
    forAll (genDouble, Gen.choose(0.0, 1.0)) { 
      (v: Double, e: Double) => 
        assert(equiv(v, v+e, abs(e*2.0))) 
    }
  }
  
  // Whether the two values are within a type specific minimal epsilon.
  property("equiv-short") {
    forAll { 
      (a: Short, b: Short) => 
        if(equiv(a, b)) 
          assert(equiv(b, a)) 
        else {
          assert(equiv(a, a))
          assert(equiv(b, b))
        }
    }
  }
  
  property("equiv-int") {
    forAll { 
      (a: Int, b: Int) => 
        if(equiv(a, b)) 
          assert(equiv(b, a)) 
        else {
          assert(equiv(a, a))
          assert(equiv(b, b))
        }
    }
  }
  
  property("equiv-long") {
    forAll { 
      (a: Long, b: Long) => 
        if(equiv(a, b)) 
          assert(equiv(b, a)) 
        else {
          assert(equiv(a, a))
          assert(equiv(b, b))
        }
    }
  }

  property("equiv-float") {
    forAll (genFloat, genFloat, genFloat) { 
      (a: Float, b: Float, c: Float) => 
        assert(equiv((a+c) * b, b*c + a*b)) 
    }
  }

  property("equiv-double") { 
    forAll (genDouble, genDouble, genDouble) { 
      (a: Double, b: Double, c: Double) => 
        assert(equiv((a+c) * b, b*c + a*b)) 
    }
  }

  // Clamp a value to be between the given upper and lower bounds.
  property("clamp-short") { 
    forAll {
      (v: Short, a: Short, b: Short) => {
        val (lower, upper) = if(a < b) (a, b) else (b, a) 
        val c = clamp(v, lower, upper) 
        assert(lower <= c)
        assert(c <= upper)
      }
    }
  }

  property("clamp-int") { 
    forAll {
      (v: Int, a: Int, b: Int) => {
        val (lower, upper) = if(a < b) (a, b) else (b, a) 
        val c = clamp(v, lower, upper) 
        assert(lower <= c)
        assert(c <= upper)
      }
    }
  }

  property("clamp-long") { 
    forAll {
      (v: Long, a: Long, b: Long) => {
        val (lower, upper) = if(a < b) (a, b) else (b, a) 
        val c = clamp(v, lower, upper) 
        assert(lower <= c)
        assert(c <= upper)
      }
    }
  }

  property("clamp-float") {
    forAll {
      (v: Float, a: Float, b: Float) => {
        val (lower, upper) = if(a < b) (a, b) else (b, a) 
        val c = clamp(v, lower, upper) 
        assert(lower <= c)
        assert(c <= upper)
      }
    }
  }

  property("clamp-double") { 
    forAll {
      (v: Double, a: Double, b: Double) => {
        val (lower, upper) = if(a < b) (a, b) else (b, a) 
        val c = clamp(v, lower, upper) 
        assert(lower <= c)
        assert(c <= upper)
      }
    }
  }

  // Linearly interpolate between two values.
  property("lerp-float") { 
    forAll (genFloat) {
      (d: Float) => {
        val (a, b) = (d, d * -10.0f) 
        val x = smoothlerp(a, b, 0.5f) 
        assert(equiv(lerp(a, b, 0.0f), a))
        assert(equiv(lerp(a, b, 1.0f), b))
        if(a < b) {
          assert(a <= x)
          assert(x <= b)
        }
        else {
          assert(b <= x)
          assert(x <= a)
        }
      }
    }
  }

  property("lerp-double") { 
    forAll (genDouble) {
      (d: Double) => {
        val (a, b) = (d, d * -10.0) 
        val x = lerp(a, b, 0.5) 
        assert(equiv(lerp(a, b, 0.0), a))
        assert(equiv(lerp(a, b, 1.0), b))
        if(a < b) {
          assert(a <= x)
          assert(x <= b)
        }
        else {
          assert(b <= x)
          assert(x <= a)
        }
      }
    }
  }
  
  // Smooth-step interpolate between two values.
  property("smoothlerp-float") { 
    forAll (genFloat) {
      (d: Float) => {
        val (a, b) = (d, d * -10.0f) 
        val x = smoothlerp(a, b, 0.5f) 
        assert(equiv(smoothlerp(a, b, 0.0f), a))
        assert(equiv(smoothlerp(a, b, 1.0f), b))
        if(a < b) {
          assert(a <= x)
          assert(x <= b)
        }
        else {
          assert(b <= x)
          assert(x <= a)
        }
      }
    }
  }

  property("smoothlerp-double") { 
    forAll (genDouble) {
      (d: Double) => {
        val (a, b) = (d, d * -10.0) 
        val x = smoothlerp(a, b, 0.5) 
        assert(equiv(smoothlerp(a, b, 0.0), a))
        assert(equiv(smoothlerp(a, b, 1.0), b))
        if(a < b) {
          assert(a <= x)
          assert(x <= b)
        }
        else {
          assert(b <= x)
          assert(x <= a)
        }
      }
    }
  }
  
  // The smooth-step interpolation function.
  {
    val g = Gen.choose(0.0, 1.0)
    property("smoothstep-float") {
      forAll (g) {
        (dv: Double) => {
          val v = dv.toFloat 
          assert(equiv(smoothstep(0.0f), 0.0f)) 
          assert(equiv(smoothstep(1.0f), 1.0f)) 
          if(v < 0.5f) 
            assert(smoothstep(v) < v) 
          else if(v > 0.5f) 
            assert(smoothstep(v) > v)
          else 
            assert(equiv(smoothstep(v), 0.5f))
        } 
      }
    }
  }

  {
    val g = Gen.choose(0.0, 1.0)
    property("smoothstep-double") {
      forAll (g) {
        (v: Double) => {
          assert(equiv(smoothstep(0.0), 0.0)) 
          assert(equiv(smoothstep(1.0), 1.0)) 
          if(v < 0.5) 
            assert(smoothstep(v) < v) 
          else if(v > 0.5) 
            assert(smoothstep(v) > v)
          else 
            assert(equiv(smoothstep(v), 0.5))
        } 
      }
    }
  }
}
