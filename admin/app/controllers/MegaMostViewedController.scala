package controllers.admin

import common.Logging
import model.{ApplicationContext, MegaSlotMeta}
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.I18nSupport
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import services.S3Megaslot

class MegaMostViewedController(
  val controllerComponents: ControllerComponents
)(implicit context: ApplicationContext)
  extends BaseController with Logging with I18nSupport {

  val slotForm = Form(
    mapping(
      "headline" -> text,
      "uk" -> text,
      "us" -> text,
      "au" -> text,
      "row" -> text
    )(MegaSlotMeta.apply)(MegaSlotMeta.unapply)
  )

  def get(): Action[AnyContent] = Action { implicit request =>
    val data = S3Megaslot.get("mega1.json").map(blob =>Json.parse(blob))

    val form = data match {
      case Some(jsValue) => slotForm.bind(jsValue)
      case None => slotForm
    }

    Ok(views.html.megaSlotForm(form))
  }

  def set(): Action[AnyContent] = Action { implicit request =>
    slotForm.bindFromRequest.fold(
      formWithErrors => {
        BadRequest(views.html.megaSlotForm(formWithErrors))
      },
      meta => {
        S3Megaslot.putPublic("mega1.json", Json.toJson(meta).toString(), "application/json")
        Redirect(routes.MegaMostViewedController.get)
      }
    )
  }
}
