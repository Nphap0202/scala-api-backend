package controllers

import javax.inject._
import models.{NewTodoListItem,  TodoListItem}
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}

import scala.collection.mutable

@Singleton
class TodoListController @Inject() (
                                     val controllerComponents: ControllerComponents
                                   ) extends BaseController {
  private val todoList = new mutable.ListBuffer[TodoListItem]()
  todoList += TodoListItem(1, "test", true)
  todoList += TodoListItem(2, "some other value", false)

  implicit val todoListJson: play.api.libs.json.OFormat[models.TodoListItem] =
    Json.format[TodoListItem]
  implicit val newTodoListJson
  : play.api.libs.json.OFormat[models.NewTodoListItem] =
    Json.format[NewTodoListItem]

  // curl.exe localhost:9000/todo
  def getAll(): Action[AnyContent] = Action {
    if (todoList.isEmpty) NoContent else Ok(Json.toJson(todoList))
  }

  // curl.exe localhost:9000/todo/1
  def getById(itemId: Long) = Action {
    val foundItem = todoList.find(_.id == itemId)
    foundItem match {
      case Some(item) => Ok(Json.toJson(item))
      case None       => NotFound
    }
  }

  // curl.exe -X PUT localhost:9000/todo/done/1
  def markAsDone(itemId: Long) = Action {
    val foundItem = todoList.find(_.id == itemId)
    foundItem match {
      case Some(item) =>
        val newItem = item.copy(isItDone = true)
        todoList.dropWhileInPlace(_.id == itemId)
        todoList += newItem
        Accepted(Json.toJson(newItem))
      case None => NotFound
    }
  }

  // curl.exe -v -X DELETE localhost:9000/todo/done
  def deleteAllDone() = Action {
    todoList.filterInPlace(_.isItDone == false)
    Accepted
  }

  // curl.exe -v --data '{"columns":["description"],"data":[["ABC"]]}'.Replace('"', '\"') -H 'Content-Type: application/json' -X POST http://localhost:9000/todo
//  def addNewItem(): Action[AnyContent] = Action { request =>
//    request.body.asJson.map { body =>
//      Json.fromJson[String](body) match {
//        case JsSuccess(task, path) =>
//          TaskListInMemoryModel.addTask(username, task);
//          Ok(Json.toJson(true))
//        case e@JsError(_) => Redirect(routes.TaskList3.load)
//      }
//      case None =>
//        BadRequest("Khong chen them gi")
//    }
//  }
  def addNewItem(): Action[AnyContent] = Action { request =>
    val content = request.body
    val jsonObject = content.asJson

    val todoListItem: Option[NewTodoListItem] =
      jsonObject.flatMap(Json.fromJson[NewTodoListItem](_).asOpt)

    todoListItem match {
      case Some(newItem) =>
        val nextId = todoList.map(_.id).max + 1
        val toBeAdded = TodoListItem(nextId, newItem.description, false)
        todoList += toBeAdded
        Created(Json.toJson(toBeAdded))
      case None =>
        BadRequest("Khong chen them gi")
    }
  }

//  def addNewItem(): Action[JsValue] = Action(parse.json) { implicit request =>
//    request.body.validate[NewTodoListItem].asOpt
//      .fold{
//        BadRequest("No item added")
//      }
//      {
//        response =>
//          val nextId = todoList.map(_.id).max +1
//          val newItemAdded = TodoListItem(nextId,response.description,false)
//          todoList += newItemAdded
//          Ok(Json.toJson(todoList))
//      }
//  }
}
