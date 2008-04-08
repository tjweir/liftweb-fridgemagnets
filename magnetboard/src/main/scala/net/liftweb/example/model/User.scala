package net.liftweb.example.model

import net.liftweb.mapper._
import DB._
import java.sql.Connection
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.util.Mailer._
import net.liftweb.http._
import S._

/* The singleton that has methods for accessing the database */  
object User extends User with MetaMegaProtoUser[User, User with KeyedMetaMapper[Long, User]] {
  override val dbTableName = "users"
  override def screenWrap = Full(<lift:surround with="default" at="content"><lift:bind /></lift:surround>)
  override def signupFields = firstName :: lastName :: email :: password :: Nil
  override val skipEmailValidation = true

  override def loginXhtml = {
    (<center>
      <form method="POST" action={S.uri}><table width="60%"><tr><td colspan="2">{S.??("log.in")}</td></tr>
      <tr><td>{S.??("email.address")}</td><td><user:email /></td></tr>
      <tr><td>{S.??("password")}</td><td><user:password /></td></tr>
      <tr><td><a href={"/"+BasePath+"/"+LostPassword}>{S.??("recover.password")}</a></td><td><user:submit /></td></tr></table>
     </form>
   </center>)
  }

}


/*** An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it */
class User extends MegaProtoUser[User] {
  def getSingleton = User // what's the "meta" server
  def primaryKeyField = id
  def super_? : Boolean = superUser
  
  // define an additional field for a personal essay
  object textArea extends MappedTextarea(this, 2048) {
    override def dbDisplay_? = false
  }
}
