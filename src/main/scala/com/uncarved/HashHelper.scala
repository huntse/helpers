/**
 * A helper for easy hashcode implementation
 *
 * Copyright (c) Sean Hunter 2009  
 *
 * This software is provided under the terms of the Scala license (see http://www.scala-lang.org/node/146)
 *
 * @author Sean Hunter
 **/
package com.uncarved.helpers

/**
 * Helper class for defining <code>hashCode</code>
 * 
 * @param seedOpt 	An optional starting seed
 * @param primeOpt	An optional prime number used when combining results
 */
class HashHelper(seedOpt: Option[Int], primeOpt: Option[Int]) {
  val prime = primeOpt match {
    case Some(p) => p
    case None    => 41
  }
  val seed = seedOpt match {
    case Some(s) => s
    case None    => 41    
  }
  var contents = seed
  
  /**
   * Mixes in an integer and returns the current hash code.
   * 
   * @param mixin The value to mix in
   */
  private def stir(mixin: Int) = { 
    contents = contents * prime + mixin
    
    contents
  } 
  
  def add(x: Boolean) = stir(if (x) 1 else 0)
  def add(x: String) = stir(x.hashCode)
  def add(x: Int) = stir(x)
  def add(x: Char) = stir(x.asInstanceOf[Int])
  def add(x: Long) = stir((x ^ (x >>> 32)).asInstanceOf[Int])
  def add(x: Float) = stir(x.hashCode)
  def add(x: Double) = stir(x.hashCode)
  
  def get = contents
  def reset = { contents = seed }
  def this() = this(None, None)
  def this(s: Int) = this(Some(s), None)
  def this(s: Int, p: Int) = this(Some(s), Some(p)) 
}
