package org.scalagfx.math

object Benchmark 
{
  class Stats(val duration: Long, val ops: Long) 
  {
    val timeInSec = duration.toDouble / 1000.0
    val timePerOp = duration.toDouble / ops.toDouble
    val opsPerSec = (ops*1000L) / duration
    
    def report() {
      println("Total Time: %.2f sec".format(timeInSec))
      println(" Total Ops: " + ops)
      println(" Time / Op: %.8f ms".format(timePerOp))
      println(" Ops / Sec: " + opsPerSec)
    }
    
    def summary(title: String) {
      println("%s (MOps/Sec): %.2f".format(title, opsPerSec.toDouble / 1000000.0))
    }
    
    def + (that: Stats) = new Stats(duration+that.duration, ops+that.ops)
  }
  
  def timeTest(title: String, iters: Int, ops: Long)(test: => Double): Stats = {
  	println("----------------------------------------------")
    print("Starting " + title + " Test: ")
    val stamp = System.currentTimeMillis
    var result = 0.0
    for(_ <- 0 until iters) 
      result = result + test
    val duration = System.currentTimeMillis - stamp
    println(" Done (%.2f)".format(result))
    println

    val stats = new Stats(duration, iters.toLong * ops)
  	stats.report
  	stats
  }
  
  /** The top-level entry method. */
  def main(args: Array[String]) {
    try {
      println("Benchmark of Math3 Vectors.")
      println
      
      print("Generating Test Data: ")
      val size = 100000
      val vecA = Array.fill(size)(Vec3d.random)
      val vecB = Array.fill(size)(Vec3d.random)
      val vecC = Array.fill(size)(Vec3d.random)
      println(" Done.")

      val iters = 10000
      
      timeTest("Warmup", iters/2, size*3) {
        var result = Vec3d(0.0)
        for(i <- 0 until size) 
          result = (vecA(i) cross vecB(i)) * (vecB(i) dot vecC(i)) - (vecA(i) / vecB(i))
        result.lengthSq
      }
      
      val add = timeTest("Add", iters, size*3) {
        var result = Vec3d(0.0)
        for(i <- 0 until size) 
          result = result + vecA(i) + vecB(i) + vecC(i)
        result.lengthSq
      }
      
      val subtract = timeTest("Subtract", iters, size*3) {
        var result = Vec3d(0.0)
        for(i <- 0 until size) 
          result = result - vecA(i) - vecB(i) - vecC(i)
        result.lengthSq
      }
      
      val multiply = timeTest("Multiply", iters, size*3) {
        var result = Vec3d(1.0)
        for(i <- 0 until size) 
          result = result * vecA(i) * vecB(i) * vecC(i)
        result.lengthSq
      }
      
      val divide = timeTest("Divide", iters, size*3) {
        var result = Vec3d(1.0)
        for(i <- 0 until size) 
          result = result / vecA(i) / vecB(i) / vecC(i)
        result.lengthSq
      }
      
      val dot = timeTest("Dot", iters, size*3) {
        var result = 0.0
        for(i <- 0 until size) 
          result = result + (vecA(i) dot vecB(i)) + (vecB(i) dot vecC(i)) + (vecC(i) dot vecA(i))
        result
      }
      
      val cross = timeTest("Cross", iters, size*3) {
        var result = Vec3d(1.0)
        for(i <- 0 until size) 
          result = result cross vecA(i) cross vecB(i) cross vecC(i)
        result.lengthSq
      }
      
      val mag = timeTest("Mag", iters, size*2) {
        var result = 0.0
        for(i <- 0 until size) 
          result = vecA(i).length + vecB(i).length + vecC(i).length
        result
      }
      
  	  println("----------------------------------------------")
      add.summary     ("     Add")
      subtract.summary(" Subract")
      multiply.summary("Multiply")
      divide.summary  ("  Divide")
      dot.summary     ("     Dot")
      cross.summary   ("   Cross")
      mag.summary     ("     Mag")      
      println
      
      (add + subtract + multiply + divide + dot + cross + mag).summary("   TOTAL")
      
      sys.exit
    }
    catch {
      case ex => 
        println("Uncaught Exception: " + ex.getMessage + "\n" +
                "Stack Trace:\n" + ex.getStackTraceString)
    }
  }
}