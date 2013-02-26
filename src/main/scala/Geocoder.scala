import io.Source
import dispatch._
import java.io.FileWriter

object Geocoder extends App {

  val input = args.headOption.getOrElse(throw new IllegalArgumentException("Input file needed as single program argument."))

  val fileWriter = new FileWriter("geocodes.csv")

  try {
    Source.fromFile(input).getLines().drop(1).foreach {
      line =>
        val address = parseAddress(line)
        val gc = geocode(address)().right
        val latLng = (for (code <- gc) yield s"${code.lat},${code.lng}").right.toOption.getOrElse("NA,NA")
        val outputLine = s"${address.postcode},${latLng}\n"
        fileWriter.write(outputLine)
    }
  } finally {
    fileWriter.close()
  }
  println("Done.")

  def geocode(address: Address) = {
    for {
      xmlEither <- fetchGeoCode(address).right
      gc <- Http.promise(extractGeocodes(xmlEither)).right
    } yield gc
  }

  def extractGeocodes(xml: scala.xml.Elem): Either[String, Geocode] = {

    val seq = (for {
      elem <- xml \\ "latLng"
      lat <- elem \ "lat"
      lng <- elem \ "lng"
    } yield Geocode(lat.text, lng.text))

    seq.headOption.toRight {
      "No geocodes found"
    }
  }

  def fetchGeoCode(address: Address): Promise[Either[String, scala.xml.Elem]] = {

    val addressXml = <address>
      <location>
        <country>
          {address.country}
        </country>
        <city>
          {address.city}
        </city>
        <postalCode>
          {address.postcode}
        </postalCode>
      </location>
      <options>
        <thumbMaps>false</thumbMaps>
      </options>
    </address>

    def mapQuestReq =
      url("http://open.mapquestapi.com/geocoding/v1/address")
        .addQueryParameter("callback", "renderOptions")
        .addQueryParameter("outFormat", "xml")
        .addQueryParameter("inFormat", "xml")
        .addQueryParameter("xml", addressXml.toString())

    val response = Http(mapQuestReq OK as.xml.Elem).either

    for (str <- response.left)
    yield "Error " + str.getMessage
  }

  case class Address(postcode: String, city: String, country: String)

  case class Geocode(lat: String, lng: String)

  def parseAddress(s: String): Address = {
    val list: Array[String] = s.split(',')
    Address(trim(list(0)), simplifyCity(trim(list(1))), trim(list(2)))
  }

  def simplifyCity(st: String) = {
    if (st.contains("ß") || st.contains(".")) ""
    else if (st.contains("-") || st.contains(" ")) st.takeWhile(c => c != '-' && c !=' ')
    else {
      st.replaceAll("ö", "o")
        .replaceAll("ä", "a")
        .replaceAll("ü", "u")
    }
  }

  def trim(s: String): String = {
    s.replaceAll("\"", "")
  }
}
