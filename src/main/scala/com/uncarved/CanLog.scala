/**
 * A helper for easy log4j usage from scala
 *
 * Copyright (c) Sean Hunter 2009  
 *
 * This software is provided under the terms of the Scala license (see http://www.scala-lang.org/node/146)
 *
 * @author Sean Hunter
 **/
package com.uncarved.helpers

import org.apache.log4j.Logger;

/**
 * CanLog is a trait you can mix in to provide easy log4j logging 
 * for your scala classes. 
 *
 * Sample usage:
 * <code><pre>
 *     class MyClass extends CanLog {
 *         def doSomething(temp: Int) = {
 *             logger.debug("About to doSomething")
 *             if(temp>100) 
 *                 logger.warn("It's mighty hot! temp: " + temp)
 *             
 *             logger.debug("Done with doSomething")
 *         }
 *     }
 * </pre></code>
 **/
trait CanLog {
	val loggerName = this.getClass.getName
    lazy val logger = Logger.getLogger(loggerName)
}

// vim: set ts=4 sw=4 noet:
