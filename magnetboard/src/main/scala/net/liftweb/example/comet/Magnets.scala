package net.liftweb.example.comet

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import net.liftweb.example.model._
import scala.xml.{NodeSeq, Text, Group}
import net.liftweb.http._
import net.liftweb.http.S
import net.liftweb.mapper._
import net.liftweb.http.S._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import java.util.Locale

class MagnetBoard(initInfo: CometActorInitInfo) extends CometActor(initInfo) {
   def defaultPrefix = "board"

  
   private var stuckOnMe: List[Magnet] = List(
     new Magnet("01", "Tyler", 40, 40), 
     new Magnet("02", "Weir", 50, 50),
     new Magnet("03", "loves", 60, 60),
     new Magnet("04", "lifted", 70, 70),
     new Magnet("05", "magnets", 80, 80),
   
   )

   def render = <div class="magnet_board">{stuckOnMe.map(_.renderAMagnet)}</div>

   override def lowPriority = {
     case AddMagnet(magnet) => stuckOnMe = magnet :: stuckOnMe ; reRender(false)
     case RemoveMagnet(magnet) => stuckOnMe = stuckOnMe.remove(_.id == magnet.id); reRender(false)
     case UpdateMagnet(magnet) => stuckOnMe = magnet :: stuckOnMe.remove(_.id == magnet.id); reRender(false)
   }
}

case class AddMagnet(magnet: Magnet)
case class RemoveMagnet(magnet: Magnet)
case class UpdateMagnet(magnet: Magnet)

class Magnet(val id: String, val message: String, val xpos: Int, val ypos: Int) {
  def renderAMagnet = <div id={id} class="magnet" style={"top: "+ypos+"; left: "+xpos}>{message}</div>
}



