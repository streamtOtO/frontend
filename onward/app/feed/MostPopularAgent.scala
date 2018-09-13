package feed

import com.gu.Box
import com.gu.contentapi.client.model.SearchQuery
import contentapi.ContentApiClient
import common._
import services.{OphanApi, S3, S3Megaslot}
import model.{Content, MegaSlotMeta, RelatedContentItem}
import play.api.libs.json._

import scala.concurrent.{ExecutionContext, Future}

object MostPopularRefresh {

  def all[A](as: Seq[A])
            (refreshOne: A => Future[Map[String, Seq[RelatedContentItem]]])
            (implicit ec: ExecutionContext): Future[Map[String, Seq[RelatedContentItem]]] = {
    as.map(refreshOne)
      .reduce( (itemsF, otherItemsF) =>
        for {
          items <- itemsF
          otherItems <- otherItemsF
        } yield items ++ otherItems
      )
  }
}

class MostPopularAgent(contentApiClient: ContentApiClient) extends Logging {

  private val agent = Box[Map[String, Seq[RelatedContentItem]]](Map.empty)

  def mostPopular(edition: Edition): Seq[RelatedContentItem] = agent().getOrElse(edition.id, Nil)

  def refresh()(implicit ec: ExecutionContext): Future[Map[String, Seq[RelatedContentItem]]] = {
    log.info("Refreshing most popular.")
    MostPopularRefresh.all(Edition.all)(refresh)
  }

  private def refresh(edition: Edition)(implicit ec: ExecutionContext): Future[Map[String, Seq[RelatedContentItem]]] =
    contentApiClient.getResponse(contentApiClient.item("/", edition)
      .showMostViewed(true)
    ).flatMap { response =>
      val mostViewed = response.mostViewed.getOrElse(Nil).take(10).map(RelatedContentItem(_))
      agent.alter(_ + (edition.id -> mostViewed))
    }

}

case class Country(code: String, edition: Edition)

class GeoMostPopularAgent(contentApiClient: ContentApiClient, ophanApi: OphanApi) extends Logging {

  private val ophanPopularAgent = Box[Map[String, Seq[RelatedContentItem]]](Map.empty)

  private val defaultCountry: Country = Country("row", Edition.defaultEdition)

  // These are the only country codes (row must be lower-case) passed to us from the fastly service.
  // This allows us to choose carefully the codes that give us the most impact. The trade-off is caching.
  private val countries = Seq(
    Country("GB", editions.Uk),
    Country("US", editions.Us),
    Country("AU", editions.Au),
    Country("CA", editions.Us),
    Country("IN", Edition.defaultEdition),
    Country("NG", Edition.defaultEdition),
    Country("NZ", editions.Au),
    defaultCountry
  )

  def mostPopular(country: String): Seq[RelatedContentItem] =
    ophanPopularAgent().getOrElse(country, ophanPopularAgent().getOrElse(defaultCountry.code, Nil))

  def refresh()(implicit ec: ExecutionContext): Future[Map[String, Seq[RelatedContentItem]]] = {
    log.info("Refreshing most popular for countries.")
    MostPopularRefresh.all(countries)(refresh)
  }

  private def refresh(country: Country)(implicit ec: ExecutionContext): Future[Map[String, Seq[RelatedContentItem]]] = {
    val ophanMostViewed = ophanApi.getMostRead(hours = 3, count = 10, country = country.code.toLowerCase)
    MostViewed.relatedContentItems(ophanMostViewed, country.edition)(contentApiClient).flatMap { items =>
      val validItems = items.flatten
      if (validItems.nonEmpty) {
        log.info(s"Geo popular ${country.code} updated successfully.")
      } else {
        log.info(s"Geo popular update for ${country.code} found nothing.")
      }
      ophanPopularAgent.alter(_ + (country.code -> validItems))
    }
  }
}

class DayMostPopularAgent(contentApiClient: ContentApiClient, ophanApi: OphanApi) extends Logging {

  private val ophanPopularAgent = Box[Map[String, Seq[RelatedContentItem]]](Map.empty)

  private val countries = Seq(
    Country("GB", editions.Uk),
    Country("US", editions.Us),
    Country("AU", editions.Au)
  )

  def mostPopular(country: String): Seq[RelatedContentItem] = ophanPopularAgent().getOrElse(country, Nil)

  def refresh()(implicit ec: ExecutionContext): Future[Map[String, Seq[RelatedContentItem]]] = {
    log.info("Refreshing most popular for the day.")
    MostPopularRefresh.all(countries)(refresh)
  }

  def refresh(country: Country)(implicit ec: ExecutionContext): Future[Map[String, Seq[RelatedContentItem]]] = {
    val ophanMostViewed = ophanApi.getMostRead(hours = 24, count = 10, country = country.code.toLowerCase())
    MostViewed.relatedContentItems(ophanMostViewed, country.edition)(contentApiClient).flatMap { items =>
      val validItems = items.flatten
      if (validItems.isEmpty) {
        log.info(s"Day popular update for ${country.code} found nothing.")
      }
      ophanPopularAgent.alter(_ + (country.code -> validItems))
    }
  }
}

case class MegaSlot(
  headline: String,
  uk: Content,
  us: Content,
  au: Content,
  row: Content
)

trait MegaSlotAgent extends Logging {
  val contentApiClient: ContentApiClient
  val s3Key: String

  private[this] val agent = Box[Option[MegaSlot]](None)

  def getHeadline: String = agent.get.map(_.headline).getOrElse("")

  def get(edition: Edition): Option[Content] = {
    log.info(s"Megaslot: looking for $edition, from ${agent.get}")
    agent.get.flatMap { megaSlot =>
      edition match {
        case editions.Uk => Some(megaSlot.uk)
        case editions.Us => Some(megaSlot.us)
        case editions.Au => Some(megaSlot.au)
        case editions.International => Some(megaSlot.row)
        case _ => None
      }
    }
  }

  def refresh()(implicit ec: ExecutionContext): Future[Option[MegaSlot]] = {
    log.info("Megaslot - refreshing.")
    val blob = S3Megaslot.get(s3Key)
    log.info(s"Megaslot - got $blob from s3")

    def populateFromCAPI(meta: MegaSlotMeta): Future[MegaSlot] = {
      val idsParam = s"${meta.uk},${meta.au},${meta.us},${meta.row}"
      val query = contentApiClient.search.ids(idsParam)

      for {
        response <- contentApiClient.getResponse(query)
      } yield {
        val models = response.results.map(c => c.id -> Content.make(c)).toMap
        log.info(s"Megaslot - capi response has IDs: ${response.results.map(_.id).mkString(",")}")
        MegaSlot(
          headline = meta.headline,
          uk = models(meta.uk),
          us = models(meta.us),
          au = models(meta.au),
          row = models(meta.row)
        )
      }
    }

    val result = for {
      str <- blob
      meta <- {
        val slot = Json.parse(str).asOpt[MegaSlotMeta]
        Json.parse(str).validate[MegaSlotMeta] match {
          case error: JsError => log.error(s"Megaslot - invalid JSON with errors: ${JsError.toJson(error).toString()}")
          case _ =>
        }
        slot
      }
    } yield populateFromCAPI(meta).map(Some.apply).flatMap(agent.alter)

    result.getOrElse(Future.successful(agent.get))
  }
}

// because of the macwire DI framework
class MegaSlot1Agent(val contentApiClient: ContentApiClient) extends MegaSlotAgent {
  override val s3Key: String = "mega1.json"
}

class MegaSlot2Agent(val contentApiClient: ContentApiClient) extends MegaSlotAgent {
  override val s3Key: String = "mega2.json"
}
