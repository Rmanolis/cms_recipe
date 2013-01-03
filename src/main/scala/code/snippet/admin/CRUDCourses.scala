package code.snippet.admin
import net.liftweb._
import common._
import util._
import http._
import scala.xml._
import Helpers._
import code.model._
import code.lib._
import net.liftweb.mapper._

class CRUDCourses extends PaginatorSnippet[Course] {
  override def count = Course.count
  override def itemsPerPage = 10
  override def page = {
    var list: List[QueryParam[Course]] = List()
    list +:= OrderBy(Course.id, Descending)
    list +:= StartAt(curPage * itemsPerPage)
    list +:= MaxRows(itemsPerPage)
    Course.findAll(list: _*)
  }
  def renderPage(in: NodeSeq): NodeSeq = {
    import SHtml._

    <table id="listCourses"> {
      <thead>
        <tr>
          <th> Name </th>
          <th> Language </th>
          <th> Edit </th>
          <th> Clean Dependents </th>
          <th> Delete Dependents </th>
          <th> Delete </th>
        </tr>
      </thead> ++
        <tbody>{
          page.map {
            course =>
              <tr> {
                <td> { course.name.is } </td> ++
                  <td>{
                    course.getLanguageName
                  } </td> ++
                  <td> {
                    button(Text("Edit"), () => {
                      S.redirectTo(Site.editCourseLoc.calcHref(course))

                    })
                  }</td> ++
                  <td> {
                    button(Text("Clean Dependents"), () => {
                      course.cleanDependents
                      S.redirectTo(Site.editCourseLoc.calcHref(course))
                    })
                  }</td> ++
                  <td> {
                    button(Text("Delete Dependents"), () => {
                      course.deleteDependents
                      S.redirectTo(Site.crudCourse.url)
                    })
                  }</td> ++
                  <td> {
                    button(Text("Delete"), () => {
                      course.delete_!
                      S.redirectTo(Site.crudCourse.url)
                    })
                  }</td>
              }</tr>
          }
        }</tbody>
    }</table>

  }

}