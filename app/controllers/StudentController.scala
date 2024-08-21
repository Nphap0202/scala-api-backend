package controllers

import models.Student._
import models.Subject._
import play.api.i18n.I18nSupport
import play.api.libs.json.Json
import play.api.mvc._

import javax.inject._

@Singleton
class StudentController @Inject()(cc: ControllerComponents) extends AbstractController(cc) with I18nSupport{

  /**
   * returns JSON of students
   */
  def studentsIndex() = Action { implicit request: Request[AnyContent] =>
    Ok(Json.toJson(students))
  }

  /**
   * returns JSON of subjects
   */
  def subjectsIndex() = Action { implicit request: Request[AnyContent] =>
    Ok(Json.toJson(subjects))
  }
}