// Copyright 2011-2012 James Michael Callahan
// See LICENSE-2.0 file for licensing information.

package org.scalagfx.opengl

import org.lwjgl.Sys

/** Factory for TimingStats. */ 
object TimingStats 
{
  /** Create new timing statistics counters. */
  def apply(): TimingStats = new TimingStats()
}

/** Time statistics. */
class TimingStats 
{ 
  /** The timestamp of the start of the frame. */
  private var stamp = Sys.getTime
  
  /** The timestamp of when FPS was last calculated. */
  private var lastFPS = Sys.getTime
  
  /** The number of frames rendered since the last FPS rate was calculated. */
  private var frameCount = 0
  
  /** The amount of FPS for each second the viewer was running. */
  private var fpsHistory: List[Int] = List()
  
  /** Update all timing statistics.
    *
    * @return The current timestamp. */
  def update: Long = {
    val stamp = Sys.getTime
    val sinceFPS = stamp - lastFPS
    if(sinceFPS > Sys.getTimerResolution) {
      fpsHistory = frameCount :: fpsHistory
      frameCount = 0
      lastFPS = stamp
    }
    frameCount = frameCount + 1
    stamp
  }
  
  /** Report timing statistics. */
  def report() {
    val avgFPS = ((0L /: fpsHistory)(_+_)).toDouble / fpsHistory.length.toDouble
    println("Average FPS: %.2f".format(avgFPS))
      
    print("FPS History:")
    for(fps <- fpsHistory) 
      print(" " + fps)
    println
  }
}
