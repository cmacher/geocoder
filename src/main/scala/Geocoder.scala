import io.Source
import dispatch._

object Geocoder extends App {


  Source.fromFile("src/addresses.csv").getLines().take(3).foreach(line => {
    if (!line.startsWith("PLZ")) {
      val address = parseAddress(line)
      for (res <- fetchGeoCode(address).right)
        println(res)
    }
  })


  def fetchGeoCode(address: Address): Promise[Either[String, String]] =  {

    val location = <location>
      <city>{address.city}</city>
      <country>{address.country}</country>
      <postalCode>{address.postcode}</postalCode>
    </location>

    def mapQuestReq =
      url("http://www.mapquestapi.com/geocoding/v1/address")
        .addQueryParameter("key","Fmjtd%7Cluub210rn0%2Cr0%3Do5-96tsd6")
        .addQueryParameter("callback","renderOptions")
        .addQueryParameter("outFormat","json")
        .addQueryParameter("inFormat","xml")
        .addQueryParameter("xml", location.text)

    val res = Http(mapQuestReq OK as.String).either

    for (str <- res.left)
        yield "Error " + str.getMessage
  }

  case class Address(postcode: String, city: String, country: String)

  def parseAddress(s: String): Address = {
    val list:Array[String] = s.split(',')
    Address(list(0), list(1), "Austria")
  }

}
