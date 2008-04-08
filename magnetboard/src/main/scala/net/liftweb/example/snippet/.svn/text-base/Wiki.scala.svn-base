package net.liftweb.example.snippet

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import net.liftweb.example.model._
import scala.xml.{NodeSeq, Text, Group}
import net.liftweb.http.S
import net.liftweb.mapper._
import net.liftweb.http.S._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.textile._

// show determines which one is used. bind hooks the content into the lift view
case class BindChoice(show: boolean, bind: () => NodeSeq)

class Wiki {
  /**
    * Display the Textile marked up wiki or an edit box
    */
  def main: NodeSeq = {
    val pageName = S.param("wiki_page") openOr "HomePage" // set the name of the page
    
    def showAll = {
      WikiEntry.findAll(OrderBy(WikiEntry.name, true)).flatMap(entry =>
      <div><a href={"/wiki/"+entry.name}>{entry.name}</a></div>)
    }

    if (pageName == "all") showAll // if the page is "all" display all the pages
    else {
      // find the entry in the database or create a new one
      val entry = WikiEntry.find(By(WikiEntry.name, pageName)) openOr WikiEntry.create.name(pageName)
      
      // is it a new entry?
      val isNew = !entry.saved_?
      
      // show edit or just display
      val edit = isNew || (S.param("param1").map(_ == "edit") openOr false)
      
      <span><a href="/wiki/all">Show All Pages</a><br/>{
	if (edit) editEntry(entry, isNew, pageName)
        else TextileParser.toHtml(entry.entry, Full((a: String) => a)) ++ 
             <br/><a href={S.uri+"/"+pageName+"/edit"}>Edit</a> // and add an "edit" link
      }</span>
    }
  }

  def choosebind(xhtml : NodeSeq) = {
    def pageName = S.param("wiki_page") openOr "HomePage" // set the name of the page

    def showAll = BindChoice((pageName == "all"), () => bind("pages", 
      (xhtml \\ "showAll").filter(_.prefix == "wiki").toList.head.child, 
      TheBindParam("all", WikiEntry.findAll(OrderBy(WikiEntry.name, true)).flatMap(entry =>
      <div><a href={"/wikibind/"+entry.name}>{entry.name}</a></div>))))

    // find the entry in the database or create a new one
    def entry = WikiEntry.find(By(WikiEntry.name, pageName)) openOr WikiEntry.create.name(pageName)

    // is it a new entry?
    def isNew = !entry.saved_?
    def toEdit = isNew || (S.param("param1").map(_ == "edit") openOr false)

    def edit = BindChoice(toEdit, () => bind("edit", 
      (xhtml \\ "editting").filter(_.prefix == "wiki").toList.head.child, 
      "form" --> editEntry(entry, isNew, pageName)))

    def view = BindChoice(!toEdit, () => bind("view", 
      (xhtml \\ "displaying").filter(_.prefix == "wiki").toList.head.child, 
      TheBindParam("name", Text(pageName)),
      TheBindParam("value", (TextileParser.toHtml(entry.entry, Full(a => a)) ++ 
		  <br/><a href={S.uri+"/"+pageName+"/edit"}>Edit</a>))))
    
    (showAll :: edit :: view :: Nil).find(_.show == true).map(_.bind()) match {
      case Some(x) => x
      case _ => <span />
    }
  }

  private def editEntry(entry: WikiEntry, isNew: boolean, pageName: String) = {
    val action = S.uri+"/"+pageName
    val message = if (isNew) Text("Create Entry named "+pageName) else Text("Edit entry named "+pageName)    
    val hobixLink = <span>&nbsp;<a href="http://hobix.com/textile/quick.html" target="_blank">Textile Markup Reference</a><br /></span>
    val cancelLink = <a href={S.uri+"/"+pageName}>Cancel</a>
    val textarea = entry.entry.toForm
    val submitButton = S.submit(isNew ? "Add" | "Edit", s => {entry.save})  
    <form method="GET" action={action}>{ // the form tag
          message ++ 
          hobixLink ++ 
          textarea ++ // display the form  
          <br /> ++ 
          cancelLink ++ 
          Text(" ") ++ 
          submitButton  
    }</form>
  }
}
