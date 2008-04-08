package net.liftweb.example.model

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import net.liftweb.mapper._
import DB._
import java.sql.Connection

/**
 * The singleton that has methods for accessing the database
 */
object WikiEntry extends WikiEntry with KeyedMetaMapper[long, WikiEntry] {
  override def dbTableName = "wiki_entry" // define the DB table name
  
  // define the order fields will appear in forms and output
  override def fieldOrder =  id :: name :: entry :: Nil
}

/**
 * An O-R mapped wiki entry
 */
class WikiEntry extends KeyedMapper[Long, WikiEntry] {
  def getSingleton = WikiEntry // what's the "meta" object
  def primaryKeyField = id

  // the primary key
  object id extends MappedLongIndex(this)
  
  // the name of the entry
  object name extends MappedString(this, 32) {
    override def dbIndexed_? = true // indexed in the DB
  }
  
  // the text of the entry
  object entry extends MappedTextarea(this, 8192) {
    override def textareaRows  = 10
    override def textareaCols = 50
  }
}
