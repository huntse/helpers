package com.uncarved.helpers.tests

import org.scalatest.Spec
import com.uncarved.helpers.http._
import org.apache.log4j._

class BasicClientSpec extends Spec {
	Logger.getLogger("org.apache").setLevel(Level.INFO)
	Logger.getLogger("com.uncarved").setLevel(Level.INFO)
	BasicConfigurator.configure()

	describe("An BasicClient object") {
		val helper = new BasicClient()	
		var str = ""
		var str2 = ""
		it("should be able to get theflautadors.org") {
			str = helper.get("http://www.theflautadors.org/")
			assert(str.length > 0)
		}

		it("should be able to reget theflautadors.org using conditional get") {
			str2 = helper.get("http://www.theflautadors.org/")
			assert(str2!="")
			assert(str2==str)
		}

		it("should be able to get HEAD of uncarved.com") {
			var head_ok = true
			try {
				helper.fetchOk(RequestType.HEAD, "http://www.uncarved.com/", List()) {
					(_) => ()
				}
			}
			catch {
				case _ => head_ok = false
			}
			assert(head_ok)
		}

		it("should be able to GET uncarved with parameters") {
			val params = List(("tag"->""), ("limit"->"5"))
			val xml = helper.getXML(Request(RequestType.GET, "http://www.uncarved.com/index.py/rss1.1.xml", params))
			val items = xml \\ "item"
			assert(items.length>0)
		}

		it("should be able to get xml") {
			val xml = helper.getXML("http://www.uncarved.com/index.py/rss1.1.xml")
			val items = xml \\ "item"
			assert(items.length>0)
		}

		it("should be able to handle redirects") {
			val testCodes = List(301, 302, 307)
			for(code <- testCodes) { 
				val url = "http://jigsaw.w3.org/HTTP/300/" + code + ".html"
				val str = helper.get(url)
				assert(str.length>0)
			}
		}

		it("should be able to do a POST with values") {
			val params = List(("your_name"->"Sean"),("fruit"->"Apple"))
			val str = helper.post("http://posttestserver.com/post.php", params)
			assert(str.length>0)
		}
	}
}


// vim: set ts=4 sw=4 noet:
