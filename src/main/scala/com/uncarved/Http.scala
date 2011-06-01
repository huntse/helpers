/**
 * A helper for easy http client implementation
 *
 * Copyright (c) Sean Hunter 2009  
 *
 * This software is provided under the terms of the Scala license (see http://www.scala-lang.org/node/146)
 *
 * @author Sean Hunter
 **/

package com.uncarved.helpers.http
import com.uncarved.helpers.CanLog
import java.io.InputStream
import java.util.zip.GZIPInputStream
import java.util.Arrays

import org.apache.http._
import org.apache.http.client._
import org.apache.http.client.methods._
import org.apache.http.client.utils.URLEncodedUtils
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.params.{HttpClientParams, CookiePolicy}
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.message.BasicNameValuePair

import org.apache.http.entity.{StringEntity,HttpEntityWrapper}
import org.apache.http.protocol.{HTTP, HttpContext, ExecutionContext}
import org.apache.http.params.{HttpProtocolParams, BasicHttpParams, HttpConnectionParams}
import org.apache.http.util.EntityUtils

import scala.io.Source
import scala.xml

/**
 * Exception class thrown when we receive a status code we don't expect.
 **/
case class StatusCode(code: Int, reason: String, contents:String)
	extends Exception("Unexpected status code: " + code + "\n" + contents)

/**
 * Enumeration of possible HTTP request types.
 **/
object RequestType extends Enumeration(0, "DELETE", "GET", "HEAD", "OPTIONS", "POST", "PUT", "TRACE") {
	type t = Value
	val DELETE, GET, HEAD, OPTIONS, POST, PUT, TRACE = Value

	def ofString(str: String) =
		str match {
			case "DELETE" => DELETE
			case "GET" => GET
			case "HEAD" => HEAD
			case "OPTIONS" => OPTIONS
			case "POST" => POST
			case "PUT" => PUT
			case "TRACE" => TRACE
			case _ => error(str + " is not a valid request type")
		}
}


/**
 * Simple wrapper entity type that takes a gzipped entity and unzips it.
 * Translated to scala from the Apache HttpClient 4 example code.
 **/
class GZIPDecompressingEntity(entity: HttpEntity) extends HttpEntityWrapper(entity) {
	override def getContent = {
		val wrappedin = wrappedEntity.getContent

		new GZIPInputStream(wrappedin)
	}

	override def getContentLength = -1
}


/**
 * Helper class that standardizes how we get request information.  This
 * enables us to simplify the calling interfaces for our requester methods
 * and also our request and response interceptors and cacheing.
 **/
case class Request(val reqType: RequestType.t, val url: String, val params: Seq[(String, String)]) {

	/**
	 * Make a Request from the parameters to a RequestInterceptor
	 **/
	def this(req: HttpUriRequest, ctx: HttpContext) = 
		this(
			RequestType.ofString(req.getMethod), 
			ctx.getAttribute(ExecutionContext.HTTP_TARGET_HOST).asInstanceOf[HttpHost].toURI +
			req.getURI.normalize.toString,
			List() //FIXME
		)

	/**
	 * Make a Request from the parameters to a ResponseInterceptor.  This 
	 * is so in the ResponseInterceptor we can cache based on request info.
	 **/
	def this(res: HttpResponse, ctx: HttpContext) = 
		this(
			ctx.getAttribute(ExecutionContext.HTTP_REQUEST).asInstanceOf[HttpUriRequest],
			ctx
		)

	/**
	 * Makes a HttpUriRequest to perform the actual http request for us
	 **/
	def toHttpRequest = {
		import RequestType._

		def getJavaParams = {
			val paramPairs = params.map(p=>new BasicNameValuePair(p._1, p._2))
			val jParams = Arrays.asList(paramPairs.toArray: _*)

			jParams
		}

		reqType match {
			case DELETE => new HttpDelete(url)
			case GET => {
				if(params.length>0) {
					val paramString = URLEncodedUtils.format(getJavaParams, HTTP.UTF_8)

					new HttpGet(url + "?" + paramString)
				}
				else
					new HttpGet(url)
			}
			case HEAD => new HttpHead(url)
			case OPTIONS => new HttpOptions(url)
			case POST => {
				val request = new HttpPost(url)
				if(params.length>0) {
					request.setEntity(new UrlEncodedFormEntity(getJavaParams, HTTP.UTF_8)); 
				}

				request
			}
			case PUT => {
				val request = new HttpPut(url) 
				if(params.length>0) {
					request.setEntity(new UrlEncodedFormEntity(getJavaParams, HTTP.UTF_8)); 
				}

				request
			}
			case TRACE => new HttpTrace(url)
			case _ => error("Unknown request type (" + reqType.toString + ") for url " + url)
		}
	}

	/**
	 * Transforms the request to a java.net.URI
	 **/
	def toURI = toHttpRequest.getURI.normalize
}

/**
 * Container for the header and content information that is cached to allow 
 * conditional get to work
 **/
case class ResponseCacheInfo() {
	var lastModified : Option[String] = None
	var eTag : Option[String] = None
	var storedEntity : Option[StringEntity] = None
}

/**
 * The type of our http response cache, used to implement conditional get
 **/
class ResponseCache extends scala.collection.mutable.HashMap[Request, ResponseCacheInfo]

/**
 * Encapsulates an HTTP response.
 **/
case class Response(val statusCode:Int, val response: HttpResponse, val entity: Option[HttpEntity])

/**
 * Interfacte for Http clients.  If this interface is implemented, we can enrich it with various
 * convenience functions
 **/
abstract class Client {
	/**
	 * override this to set the User-Agent header on outgoing requests.
	 **/
	val userAgent = this.getClass.getName

	/**
	 * override this to set the connection timeout
	 **/
	val connectionTimeout = 20000

	/**
	 * override this to set the socket timeout
	 **/
	val soTimeout = 3000

	/**
	 * Cache used by the conditional get mechanism
	 **/
	var cache = new ResponseCache

	/**
	 * derived classes must define how they create and configure the client
	 */
	protected val client : DefaultHttpClient

	/**
	 * Execute a request and run some code with the results.
	 *
	 * @param req   The http request (in the form of a Request)
	 * @param thunk A code snippet that takes a response code, the response and
	 *              an entity option that will be None if the response didn't result
	 *              in an entity
	 **/
	def fetch[T](req: Request)(thunk: Response => T) : T

	/**
	 * Execute a request and run some code with the results if the response 
	 * code is "OK" (200 to 204).
	 *
	 * @param req   The http request (in the form of a Request)
	 * @param thunk A code snippet that takes a response code, the response and
	 *              an entity option that will be None if the response didn't result
	 *              in an entity
	 **/
	def fetchOk[T](req: Request)(thunk: Response => T) : T

	/**
	 * Translate an entity option into a string.  Uses UTF-8 as a 
	 * default encoding if the entity doesn't set a charset in its
	 * content-type header
	 **/
	protected def entityString(entity: Option[HttpEntity]) =
		entity match { 
			case None => ""
			case Some(ent) => EntityUtils.toString(ent, HTTP.UTF_8)
		}

	/**
	 * Shut the client connection manager down, freeing all connection resources
	 **/
	def shutdown {
		(client getConnectionManager) shutdown
	}
}

/**
 * Trait which uses the interface above to provide a rich and convenient http
 * client interface.
 */
trait RichClient extends Client {
	/**
	 * Execute a request and run some code with the results if the response 
	 * code is "OK" (200 to 204).
	 *
	 * @param reqType The http request type
	 * @param path    The url
	 * @param thunk   A code snippet that takes a response code, the response and
	 *                an entity option that will be None if the response didn't result
	 *                in an entity
	 * @param params  The request parameters
	 **/
	def fetchOk[T](reqType: RequestType.t, path: String, params: Seq[(String, String)])(thunk: Response => T) : T =
		fetchOk(Request(reqType, path, params))(thunk)

	/**
	 * Do an http requst and return the results as a string
	 *
	 * @param req The request
	 **/
	def getString(req: Request) = {
		fetchOk(req) {
			case Response(_, _, ent) => entityString(ent)
		}
	}

	/**
	 * Do an http requst and return the results as a string
	 *
	 * @param reqType The http request type
	 * @param path    The url
	 * @param params  The request parameters
	 **/
	def getString(reqType: RequestType.t, path: String, params: Seq[(String, String)]) : String = 
		getString(Request(reqType, path, params))

	/**
	 * Do an http GET and return the results as a string
	 *
	 * @param path The url
	 **/
	def get(path: String) = getString(RequestType.GET, path, Seq()) 

	/**
	 * Do an http GET and return the results as a string
	 *
	 * @param path The url
	 * @param params The request parameters
	 **/
	def get(path: String, params: Seq[(String, String)]) : String = getString(RequestType.GET, path, params) 

	/**
	 * Do an http POST and return the results as a string
	 *
	 * @param path The url
	 * @param params The request parameters
	 **/
	def post(path: String, params: Seq[(String, String)]) : String = getString(RequestType.POST, path, params)

	/**
	 * Do an http request and return the response entity contents
	 *
	 * @param req The request helper for the reqest
	 **/
	def getContent(req: Request) = {
		fetchOk(req) {
			case Response(_, _, Some(ent)) => ent.getContent()
			case Response(_, _, None)      => error("Attempt to get empty content")
		} 
	}

	/**
	 * Do an http request and return the response entity contents
	 *
	 * @param reqType The http request type
	 * @param path The url
	 * @param params The request parameters
	 **/
	def getContent(reqType: RequestType.t, path: String, params: Seq[(String, String)]) : InputStream = 
		getContent(Request(reqType, path, params))

	/**
	 * Do an http GET request and return the response entity contents
	 *
	 * @param path The url
	 * @param params The request parameters
	 **/
	def getContent(path: String, params: Seq[(String, String)]) : InputStream = 
		getContent(RequestType.GET, path, params)
	
	/**
	 * Do an http request and return a scala IO source
	 *
	 * @param req The http request 
	 * @param params The request parameters
	 **/
	def getSource(req: Request) = Source.fromInputStream(getContent(req), HTTP.UTF_8)

	/**
	 * Do an http request and return a scala IO source
	 *
	 * @param reqType The http request type
	 * @param path The url
	 * @param params The request parameters
	 **/
	def getSource(reqType: RequestType.t, path: String, params: Seq[(String, String)]) : scala.io.Source = 
		getSource(Request(reqType, path, params))

	/**
	 * Do an http GET request and return a scala IO source
	 *
	 * @param path The url
	 * @param params The request parameters
	 **/
	def getSource(path: String, params: Seq[(String, String)]) : scala.io.Source = 
		getSource(RequestType.GET, path, params)

	/**
	 * Do an http request and parse the results as XML and return the XML sequence
	 *
	 * @param req The request helper for the request
	 **/
	def getXML(req: Request) = xml.XML.load(getContent(req))

	/**
	 * Do an http request and parse the results as XML and return the XML sequence
	 *
	 * @param reqType The http request type
	 * @param path The url
	 * @param params The request parameters
	 **/
	def getXML(reqType: RequestType.t, path: String, params: Seq[(String, String)]) : xml.Elem = 
		getXML(Request(reqType, path, params))

	/**
	 * Do an http GET request and parse the results as XML and return the XML sequence
	 *
	 * @param path The url
	 * @param params The request parameters
	 **/
	def getXML(path: String, params: Seq[(String, String)]) : xml.Elem = getXML(RequestType.GET, path, params)
	
	/**
	 * Do an http GET request and parse the results as XML and return the XML sequence
	 *
	 * @param path The url
	 **/
	def getXML(path: String) : xml.Elem = getXML(RequestType.GET, path, Seq())
	
}

/**
 * Class that allows simple and scalable http requests.  It wraps up all of the complexity of
 * HttpClient 4 and HttpCore 4 so you get cookies, redirection, transparent gzip support, and 
 * condition get Last Modified and ETag support.
 *
 * Sample usage:
 *
 * <code><pre>
 *     import com.uncarved.helpers.http._
 *
 *     val helper = new BasicClient()
 *     //Get a webpage as a string (if you look at the apache http log4j messages you can see it
 *     //does conditional get and has transparent gzip support)
 *     val str = helper.get("http://www.theflautadors.org/")
 *
 *     //Get some XML (with request parameters supplied)
 *     val params = List(("tag"->""), ("limit"->"5"))
 *     val xml = helper.getXML(Request(RequestType.GET, "http://www.uncarved.com/index.py/rss1.1.xml", params))
 *     val items = xml \\ "item"					  
 * </pre></code>
 **/
class BasicClient extends RichClient with CanLog {
	/**
	 * This class exends the HttpClient defaults by setting various parameters
	 * to more useful settings.
	 **/
	class ConfiguredHttpClient extends DefaultHttpClient with CanLog { 
		override def createHttpParams = {
			logger.debug("Creating Http Params")

			val params = new BasicHttpParams
			HttpProtocolParams.setVersion(params, HttpVersion.HTTP_1_1)
			HttpProtocolParams.setContentCharset(params, HTTP.UTF_8)
			HttpProtocolParams.setUseExpectContinue(params, false)
			HttpProtocolParams.setUserAgent(params, userAgent)
			HttpConnectionParams.setConnectionTimeout(params, connectionTimeout)
			HttpConnectionParams.setSoTimeout(params, soTimeout)
			HttpClientParams.setCookiePolicy(params, CookiePolicy.BROWSER_COMPATIBILITY)
			logger.debug("Done creating Http Params")
			params
		}
	}

	/**
	 * This class is used to intercept outgoing Http requests and modify
	 * them.  This enables us to do conditional get and transparent gzip
	 * encoding so we are slightly more polite web citizens.
	 **/
	class RequestInterceptor extends HttpRequestInterceptor with CanLog {

		/**
		 * Intercepts a single outgoing request and modifies it to add
		 * headers to allow conditional get and transparent gzip support.
		 *
		 * @param req the http request itself
		 * @param ctx some context information
		 **/
		def process(req: HttpRequest, ctx: HttpContext) {
			logger.debug("Processing request")
			if(!req.containsHeader("Accept-Encoding"))
				req addHeader("Accept-Encoding", "gzip")

			val key = new Request(req.asInstanceOf[HttpUriRequest], ctx)
			logger.debug("Request key is " + key)
			if(cache.contains(key)) {
				logger.debug("key found in cache")
				val hit = cache(key)
				if(hit.storedEntity.isDefined) {
					hit.eTag match {
						case Some(tag) => req addHeader("If-None-Match", tag)
						case None => ()
					}

					hit.lastModified match {
						case Some(since) => req addHeader("If-Modified-Since", since)
						case None => ()
					}
				}
			}

			logger.debug("Done processing request")
		}
	}

	/**
	 * This class is used to intercept incoming Http responses and modify
	 * them.  This enables us to do conditional get and transparent gzip
	 * encoding so we are slightly more polite web citizens.
	 **/
	class ResponseInterceptor extends HttpResponseInterceptor with CanLog { 

		/**
		 * Intercepts a single incoming response and modifies it to add
		 * headers to allow conditional get and transparent gzip support.
		 *
		 * @param req the http request itself
		 * @param ctx some context information
		 **/
		def process(res: HttpResponse, ctx: HttpContext) {
			logger.debug("Processing response")
			val status = res.getStatusLine
			val code = status.getStatusCode
			logger.debug("got response " + code + " " + status.getReasonPhrase)
			val key = new Request(res, ctx)
			logger.debug("Request key from response is " + key)
			if((200 to 204) contains code) {
				var cacheInfo = ResponseCacheInfo()
				if(res containsHeader "Last-modified") {
					val lmHeader = (res getHeaders "Last-modified")(0)
					cacheInfo.lastModified = Some(lmHeader getValue)
				}

				if(res containsHeader "ETag") {
					val etagHeader = (res getHeaders "ETag")(0)
					cacheInfo.eTag = Some(etagHeader getValue)
				}
				
				cacheInfo.storedEntity = 
					res.getEntity match { 
						case null => None
						case ent  => {
							val ceHeader = ent.getContentEncoding
							val contents = 
								if(ceHeader == null) 
									EntityUtils.toString(ent, HTTP.UTF_8)
								else {
									logger.debug("unpacking gzipped content")
									val codecs = ceHeader.getElements
									if(codecs.exists(c => c.getName.equalsIgnoreCase("gzip")))
										EntityUtils.toString(new GZIPDecompressingEntity(ent), HTTP.UTF_8)
									else {
										logger.warn("Content encoding header found, but no gzip codec.")
										EntityUtils.toString(ent, HTTP.UTF_8)
									}
								}

							Some(new StringEntity(contents))
						}
					}
				cacheInfo.storedEntity match {
					case Some(ent) => res.setEntity(ent)
					case None => ()
				}
				cache += ((key, cacheInfo))
			}
			else if(code == 304) {
				logger.debug("304 Not modified so we pass the cached entity")
				val hit = cache(key)
				assert(hit.storedEntity.isDefined)
				res.setEntity(hit.storedEntity.get)
				val ver = status.getProtocolVersion()
				res.setStatusLine(ver, 200)
			}
			logger.debug("done processing response")
		}
	}

	protected val client = {
		logger.debug("Configuring http client")
		val thisClient = new ConfiguredHttpClient
		thisClient.addRequestInterceptor(new RequestInterceptor)
		thisClient.addResponseInterceptor(new ResponseInterceptor)
		logger.debug("done configuring http client")
		thisClient
	}

	/**
	 * Execute a request and run some code with the results.
	 *
	 * @param req   The http request (in the form of a Request)
	 * @param thunk A code snippet that takes a response code, the response and
	 *              an entity option that will be None if the response didn't result
	 *              in an entity
	 **/
	def fetch[T](req: Request)(thunk: Response => T) = {
		logger.debug("Beginning fetch")
		val result = client.execute(req.toHttpRequest)
		val code = result.getStatusLine.getStatusCode
		val ent = 
			result.getEntity match {
				case null => None
				case e => Some(e)
			}
		try {
			logger.debug("done fetch, starting thunk")
			val res = thunk(Response(code, result, ent))
			logger.debug("done thunk")

			res
		}
		finally {
			ent.foreach(EntityUtils.consume(_)) 
		}
	}

	/**
	 * Execute a request and run some code with the results if the response 
	 * code is "OK" (200 to 204).
	 *
	 * @param req   The http request (in the form of a Request)
	 * @param thunk A code snippet that takes a response code, the response and
	 *              an entity option that will be None if the response didn't result
	 *              in an entity
	 **/
	def fetchOk[T](req: Request)(thunk: Response => T) : T = {
		fetch(req) {
			r =>
				if((200 to 204) contains r.statusCode)
					thunk(r)
				else {
					val reason = r.response.getStatusLine.getReasonPhrase
					throw StatusCode(r.statusCode, reason, entityString(r.entity))
				}
		}
	}
}

// vim: set ts=4 sw=4 noet:
