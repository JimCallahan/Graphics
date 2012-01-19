package org.scalagfx.math

//--------------------------------------------------------------------------------------------------
//   I N T E R V A L
//--------------------------------------------------------------------------------------------------

/** Companion object for Range. */
object Interval
{
  /** Create an interval: [lower, upper)
    * 
    * @param lower The inclusive lower bounds.
    * @param upper The exclusive upper bounds. */
  def apply[T](lower: T, upper: T)
              (implicit orderer: T => Ordered[T]) : Interval[T] = 
    new Interval(lower, true, upper, false)
  
  /** Create an interval with specific boundary conditions.
    *
    * @param lower The lower bounds.
    * @param lowerIncl Whether the lower bounds is inclusive (or exclusive).
    * @param upper The upper bounds.
    * @param upperIncl Whether the upper bounds is inclusive (or exclusive). */ 
  def apply[T](lower: T, lowerIncl: Boolean, upper: T, upperIncl: Boolean)
              (implicit orderer: T => Ordered[T]): Interval[T] = 
    new Interval(lower, lowerIncl, upper, upperIncl)
}

/** An immutable interval of values.
  *
  * @constructor Create a new interval.
  * @param lower The lower bounds.
  * @param isLowerInclusive Whether the lower bounds is inclusive (or exclusive).
  * @param upper The upper bounds.
  * @param isUpperInclusive Whether the upper bounds is inclusive (or exclusive). */ 
class Interval[T](val lower: T, val isLowerInclusive: Boolean, 
                  val upper: T, val isUpperInclusive: Boolean)
                 (implicit orderer: T => Ordered[T]) 
{    
  //------------------------------------------------------------------------------------------------
  //   C O M P A R I S O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Whether the value is below the lower bounds. */ 
  def isBelow(v: T) = if(isLowerInclusive) v < lower else v <= lower
  
  /** Whether the value is above the upper bounds. */ 
  def isAbove(v: T) = if(isUpperInclusive) v > upper else v >= upper

  /** Whether the value is within the interval. */ 
  def isInside(v: T) = !isBelow(v) && !isAbove(v)
  

  //------------------------------------------------------------------------------------------------
  //   O P S 
  //------------------------------------------------------------------------------------------------

  /** Create an interval with a lower bounds equal to (but not intersecting) the current
    * upper bounds.
    * 
    * @param newUpper The new upper bounds.
    * @param upperIncl Whether the new upper bounds is inclusive (or exclusive). */
  def shiftUp(newUpper: T, upperIncl: Boolean = false) = 
    Interval(upper, !isUpperInclusive, newUpper, upperIncl)

  /** Create an interval with an upper bounds equal to (but not intersecting) the current
    * lower bounds.
    * 
    * @param newLower The new lower bounds.
    * @param lowerIncl Whether the new lower bounds is inclusive (or exclusive). */
  def shiftDown(newLower: T, lowerIncl: Boolean = true) = 
    Interval(newLower, lowerIncl, lower, !isLowerInclusive)

  

  //------------------------------------------------------------------------------------------------
  //   C O N V E R S I O N                                                                  
  //------------------------------------------------------------------------------------------------

  /** Convert to a string representation. */
  override def toString() = 
    ("Interval" + 
     (if(isLowerInclusive) "[" else "(") + 
     lower + ", " + upper + 
     (if(isUpperInclusive) "]" else ")"))
}

