// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.math

import scala.math.{max,ulp}

//--------------------------------------------------------------------------------------------------
//   S C A L A R
//--------------------------------------------------------------------------------------------------

/** A collection of convienence methods for scalar values. */ 
object Scalar 
{
  /** Whether the two values are within a given epsilon of each other. */ 
  def equiv(a: Short, b: Short, epsilon: Short): Boolean = 
    if(a < b) a+epsilon >= b 
    else b+epsilon >= a

  /** Whether the two values are within a given epsilon of each other. */ 
  def equiv(a: Int, b: Int, epsilon: Int): Boolean = 
    if(a < b) a+epsilon >= b 
    else b+epsilon >= a

  /** Whether the two values are within a given epsilon of each other. */ 
  def equiv(a: Long, b: Long, epsilon: Long): Boolean = 
    if(a < b) a+epsilon >= b 
    else b+epsilon >= a
  
  /** Whether the two values are within a given epsilon of each other. */ 
  def equiv(a: Float, b: Float, epsilon: Float): Boolean = 
    if(a < b) a+epsilon >= b 
    else b+epsilon >= a
  
  /** Whether the two values are within a given epsilon of each other. */ 
  def equiv(a: Double, b: Double, epsilon: Double): Boolean = 
    if(a < b) a+epsilon >= b 
    else b+epsilon >= a

  
  /** Whether the two values are within a type specific minimal epsilon. */ 
  def equiv(a: Short, b: Short): Boolean = 
    a == b
  
  /** Whether the two values are within a type specific minimal epsilon. */ 
  def equiv(a: Int, b: Int): Boolean = 
    a == b
  
  /** Whether the two values are within a type specific minimal epsilon. */ 
  def equiv(a: Long, b: Long): Boolean = 
    a == b
    
  /** Whether the two values are within a type specific minimal epsilon. */ 
  def equiv(a: Float, b: Float): Boolean = 
    equiv(a, b, max(ulp(a), ulp(b))*1.0E3f) 
    
  /** Whether the two values are within a type specific minimal epsilon. */ 
  def equiv(a: Double, b: Double): Boolean = 
    equiv(a, b, max(ulp(a), ulp(b))*1.0E4) 


  /** Clamp a value to be between the given upper and lower bounds. */
  def clamp(v: Short, lower: Short, upper: Short): Short = 
    if(v < lower) lower else if(v > upper) upper else v

  /** Clamp a value to be between the given upper and lower bounds. */
  def clamp(v: Int, lower: Int, upper: Int): Int = 
    if(v < lower) lower else if(v > upper) upper else v

  /** Clamp a value to be between the given upper and lower bounds. */
  def clamp(v: Long, lower: Long, upper: Long): Long = 
    if(v < lower) lower else if(v > upper) upper else v

  /** Clamp a value to be between the given upper and lower bounds. */
  def clamp(v: Float, lower: Float, upper: Float): Float = 
    if(v < lower) lower else if(v > upper) upper else v

  /** Clamp a value to be between the given upper and lower bounds. */
  def clamp(v: Double, lower: Double, upper: Double): Double = 
    if(v < lower) lower else if(v > upper) upper else v

  
  /** Linearly interpolate between two values. */
  def lerp(a: Float, b: Float, t: Float): Float = 
    a + t*(b - a)
  
  /** Linearly interpolate between two values. */
  def lerp(a: Double, b: Double, t: Double): Double = 
    a + t*(b - a)


  /** Smooth-step interpolate between two values. */
  def smoothlerp(a: Float, b: Float, t: Float): Float = 
    lerp(a, b, smoothstep(t))

  /** Smooth-step interpolate between two values. */
  def smoothlerp(a: Double, b: Double, t: Double): Double = 
    lerp(a, b, smoothstep(t))
     

  /** The smooth-step interpolation function. */
  def smoothstep(t: Float): Float = 
    (3.0f*t*t) - (2.0f*t*t*t)

  /** The smooth-step interpolation function. */
  def smoothstep(t: Double): Double = 
    (3.0*t*t) - (2.0*t*t*t)
}
