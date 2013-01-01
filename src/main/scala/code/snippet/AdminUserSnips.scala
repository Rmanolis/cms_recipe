package code.snippet
import scala.xml.{ NodeSeq, Text }
import net.liftweb._
import util._
import http._
import common._
import mapper._
import Helpers._

import js.JsCmds._
import code.model._
import code.lib._

class CRUDUsers extends PaginatorSnippet[User] {
  override def count = Language.count
  override def itemsPerPage = 10
  override def page = {
    var list: List[QueryParam[User]] = List()
    list +:= OrderBy(User.id, Descending)
    list +:= StartAt(curPage * itemsPerPage)
    list +:= MaxRows(itemsPerPage)
    User.findAll(list: _*)
  }
  def renderPage(in: NodeSeq): NodeSeq = {
    import SHtml._
    <table id="listUsers"> {
      <tr>
        <th> Name </th>
        <th> Admin </th>
        <th> Make Admin </th>
        <th> Clean Dependents </th>
        <th> Delete Dependents </th>
        <th> Delete </th>
      </tr> ++
        page.map {
          user =>
            val makeHimAdminOrNot_? = if (user.superUser.is) {
              Text("Remove as Admin")
            } else {
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
                  button(Text("Clean deps"), () => {
                    user.cleanDependents
                    S.redirectTo(Site.crudUser.url)
                  })
                }</td> ++
                <td> {
                  button(Text("Delete deps"), () => {
                    user.deleteDependents
                    S.redirectTo(Site.crudUser.url)
                  })
                }</td> ++
                <td> {
                  button(Text("Delete"), () => {
                    user.delete
                    S.redirectTo(Site.crudUser.url)
                  })
                }</td>
            }</tr>
        }
    }</table>

  }
}