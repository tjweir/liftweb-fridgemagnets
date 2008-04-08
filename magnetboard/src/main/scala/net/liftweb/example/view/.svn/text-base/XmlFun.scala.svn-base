package net.liftweb.example.view

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import scala.xml.{Text, Node, NodeSeq}
import net.liftweb.http.S
import net.liftweb.http.S._

class XmlFun {
  def render = {
    val addresses = addressNode("123 any street", null, "SF", "CA", "94122", "US") ::
    addressNode("456 other lane", "flat 3", "London", "", "NW3", "GB") ::
    addressNode("14 gordon st", "#204", "Brighton", "MA", "02135", "US") ::
    addressNode("37 foo lane", null, "Ixtapa", "MX", "ABC", "MX") :: 
    addressNode("44 sheep st", "#1", "Liverpool", "", "GE1", "GB") :: 
    addressNode("74 nice st", "#1801", "Chicago", "IL", "60606", "US") :: Nil

val toCount = param("country") openOr {"US"}
    <lift:surround with="default" at="content">

<p>The XML is 
<pre>{addresses.map{e => Text(e.toString) :: <br/> :: Nil}}
</pre></p>
<p>The count for {toCount} nodes is {countryCount(toCount, addresses)}</p>

<p><a href='/xml_fun'>Count US addresses.</a></p>
<p><a href='/xml_fun?country=GB'>Count GB addresses.</a></p>
    
    </lift:surround>
  }
  
  private def addressNode(line1 : String,line2 : String, city : String, state : String, zip_pc : String, country : String) = {
    <address>
      <line>{line1}</line>{
      if (line2 != null && line2.length > 0) <line>{line2}</line> else Text("")}
      <city>{city}</city>  <state>{state}</state> <country>{country}</country> 
    </address>
        }
      
      private def countryCount(toMatch : String, xml : NodeSeq) = {
         (for (addr <- xml \\ "address"; country <- addr \\ "country" ; if country.text == toMatch) yield country).length
      }
}
