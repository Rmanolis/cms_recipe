package code.snippet
import scala.xml.{ NodeSeq, Text }
import net.liftweb._
import util._
import http._
import common._
import Helpers._

import js.JsCmds._
import code.model._
import code.lib._

class CRUDUsers {
  def render = {
    import SHtml._
    <table id="listUsers"> {
      <tr>
        <th> Name </th>
        <th> Admin </th>
        <th> Make Admin </th>
        <th> Delete </th>
      </tr> ++
        User.findAll.map {
          user =>
            val makeHimAdminOrNot_? = if(user.superUser.is) {
              Text("Remove as Admin")
            }else{
              Text("Make Admin")
            }
            <tr> {
              <td> { user.niceNameWEmailLink } </td> ++
              <td> { user.superUser.is } </td> ++
                <td> {
                  button(makeHimAdminOrNot_?, () => {
                    user.autoChoosePrevileges
                    S.redirectTo(Site.crudUser.url)

                  })
                }</td> ++
                <td> {
                  button(Text("Delete"), () => {
                    user.delete_!
                    S.redirectTo(Site.crudUser.url)
                  })
                }</td>
            }</tr>
        }
    }</table>

  }
}