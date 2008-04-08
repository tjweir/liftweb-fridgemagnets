package net.liftweb.example.comet

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import scala.actors.Actor
import scala.actors.Actor._
import net.liftweb.util.Helpers._
import scala.xml.{NodeSeq}
import scala.collection.immutable.TreeMap
import net.liftweb.textile.TextileParser
import scala.xml.Text
import java.util.Date

/**
 * A chat server.  It gets messages and returns them
 */

class ChatServer extends Actor {
  def act = loop(Nil, Nil)
  
  /**
    * Convert an incoming string into XHTML using Textile Markup
    *
    * @param msg the incoming string
    *
    * @return textile markup for the incoming string
    */
  def toHtml(msg: String): NodeSeq = TextileParser.parse(msg, a => a). // parse it
      map(_.toHtml.toList match {case Nil => Nil case x :: xs => x.child}).  // convert to html and get the first child (to avoid things being wrapped in <p>)
      getOrElse(Text(msg)) // if it wasn't parsable, then just return a Text node of the message
  
  def loop(chat: List[ChatLine], sessions: List[Actor]) {
    react {
    case ChatServerMsg(user, msg) => 
      val chatu = (ChatLine(user, toHtml(msg), timeNow) :: chat).take(500)
      val toDistribute = chatu.take(15)
      sessions.foreach (_ ! ChatServerUpdate(toDistribute))
      loop(chatu, sessions)

    case ChatServerAdd(me) => 
      reply(ChatServerUpdate(chat.take(15)))
      loop(chat, me :: sessions)

    case ChatServerRemove(me) => loop(chat, sessions.remove(_ == me))

    case _ => loop(chat, sessions)
  }
  }
}

object ChatServer {
  val server = {
    val ret = new ChatServer
    ret.start
    ret
  }
}

case class ChatLine(user: String, msg: NodeSeq, when: Date)
case class ChatServerMsg(user: String, msg: String)
case class ChatServerUpdate(msgs: List[ChatLine])
case class ChatServerAdd(me: Actor)
case class ChatServerRemove(me: Actor)

