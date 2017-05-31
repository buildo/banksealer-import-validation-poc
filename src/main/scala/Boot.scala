import java.time.OffsetDateTime

import checklist._, Rule._, Message._

import cats._, cats.data._, cats.implicits._

import io.buildo.enumero.annotations._

object DriverController extends App with SlickDataModule with ConverterModule with ValidationModule with EnrichTransactionModule {
  import Models._

  val bonifici: List[Bonifico] = retrieve
  val validatedBonifici: List[Checked[Bonifico]] = validate(bonifici)

  //distinguish transactions to be discarded from valid transactions
  val (validBonifici, toBeDiscardedBonifici) = validatedBonifici.partition { bonifico =>
    bonifico match {
      case Ior.Left(left) => !(left exists (_.isError))
      case Ior.Both(left, _) => !(left exists (_.isError))
      case Ior.Right(_) => true
    }
  }

  //write on db transactions to be discarded
  toBeDiscardedBonifici map Helper.writeErrorOnDb

  //clean warnings and return pure List[Bonifico]
  val cleanBonifici: List[Bonifico] = validBonifici map Helper.cleanWarnings

  //enrich valid transactions with values
  val enrichedBonifici = cleanBonifici map enrich

  //convert in proper format for core
  val convertedBonifici = enrichedBonifici map convert

  convertedBonifici map Helper.writeOnCoreDb
}

trait EnrichTransactionModule {
  import Models._

  val enrich = { bonifico: Bonifico =>
    EnrichedBonifico(
      id = bonifico.id,
      userId = bonifico.userId,
      datetime = bonifico.datetime,
      ip = bonifico.ip,
      importo = bonifico.importo,
      tipo = bonifico.tipo,
      asn = extractAsn(bonifico.ip)
    )
  }

  def extractAsn(ip: Option[String]) = ip match {
    case Some(v) => Some("AS7786")
    case None => None
  }
}

object Helper {
  import Models._
  val writeErrorOnDb = { event: Checked[Bonifico] =>
    val errors = event match {
      case Ior.Left(left) => left.filter(_.isError)
      case Ior.Both(left, _) => left.filter(_.isError)
      case _ => throw new Exception("nope")
    }

    println("writing on errors database", errors)
  }

  val writeOnCoreDb = { event: Map[String, String] =>
    println("writing on core database", event)
  }

  val cleanWarnings = { bonifico: Checked[Bonifico] =>
    bonifico match {
      case Ior.Left(_) => throw new Exception("There shouldnt be Left here")
      case Ior.Both(errors, result) =>
        println("log", errors)
        result
      case Ior.Right(result) =>
        result
    }
  }
}

trait ValidationModule {
  import Models._

  def validate(bonifici: List[Bonifico]): List[Checked[Bonifico]] =
    bonifici map (checkBonifico(_))

  val checkIp: Rule[Option[String], Option[String]] = Rule.pure { 
    case Some(ip) =>
      if (!ip.split('.').map(_.toInt < 256).foldLeft(true)(_ && _)) Ior.both(warnings("Ip is not valid - converting to None"), None)
      else Ior.right(Some(ip))
    case None =>
      Ior.right(None)
  }

  val checkTipo: Rule[Option[String], Option[String]] = Rule.pure {
    case Some(tipo) =>
      TipoBonifico.caseFromString(tipo) match {
        case Some(t) => Ior.right(Some(tipo))
        case None => Ior.left(errors(s"couldn't derive case from tipo $tipo"))
      }
    case None => Ior.right(None)
  }

  val checkBonifico: Rule[Bonifico, Bonifico] =
    Rule[Bonifico]
      .field(_.userId)(nonEmpty)
      .field(_.ip)(checkIp)
}

trait ConverterModule {
  import Models._

  //already implemented in driver as `raw2transaction` (it's a macro)
  val convert = { bonifico: EnrichedBonifico =>
    Map(
      "id" -> bonifico.id.toString,
      "userId" -> bonifico.userId.toString,
      "datetime" -> bonifico.datetime.toString,
      "ip" -> bonifico.ip.toString,
      "importo" -> bonifico.importo.toString,
      "tipo" -> bonifico.tipo.toString,
      "asn" -> bonifico.asn.toString
    )
  }
}

trait SlickDataModule {
  import Models._
  def retrieve: List[Bonifico] = List(
    Bonifico(
      id = 1,
      userId = "user1",
      datetime = OffsetDateTime.now.minusMinutes(10),
      ip = Some("10.0.0.1"),
      importo = Some(10.0),
      tipo = Some("SEP")
    ),
    Bonifico(
      id = 2,
      userId = "user2",
      datetime = OffsetDateTime.now.minusMinutes(7),
      ip = Some("10.0.0.3333"),
      importo = Some(10.0),
      tipo = Some("SEPA")
    ),
    Bonifico(
      id = 3,
      userId = "",
      datetime = OffsetDateTime.now.minusMinutes(7),
      ip = Some("10.0.0.256"),
      importo = Some(100.0),
      tipo = Some("Giroconto")
    ),
    Bonifico(
      id = 4,
      userId = "",
      datetime = OffsetDateTime.now.minusMinutes(7),
      ip = Some("10.0.0.25"),
      importo = Some(100.0),
      tipo = Some("Giroconto")
    )
  )
}

object Models {
  @enum trait TipoBonifico {
    object Giroconto
    object SEPA
  }

  case class Bonifico(
    id: Int,
    userId: String,
    datetime: OffsetDateTime,
    ip: Option[String],
    importo: Option[Double],
    tipo: Option[String]
  )

  case class EnrichedBonifico(
    id: Int,
    userId: String,
    datetime: OffsetDateTime,
    ip: Option[String],
    importo: Option[Double],
    tipo: Option[String],
    asn: Option[String]
  )
}
