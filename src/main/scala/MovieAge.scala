import dispatch._
import Defaults._
import argonaut._, Argonaut._
import scalaz.ValidationNel
import scalaz.\/
import scalaz.syntax.validation._ 

object MovieAge {
	val apiKeySouce = io.Source.fromFile(".api_key")
	val apiKey = apiKeySouce.mkString
	apiKeySouce.close()

	def main(args: Array[String]) {
		getCurrentMovies.map(getMovieAvg(_))
		getCurrentMovies.map(println(_))
	}

	def constructFreebaseQuery(query: String) : dispatch.Req = {
		val freebase = host("www.googleapis.com").secure / "freebase" / "v1" / "mqlread"
		freebase <<? Map("query" -> query) <<? Map("key" -> apiKey)
	}

	//currently reads movies from file, TODO: get current top box office movies
	//from some source
	def getCurrentMovies() : List[String] = {
		io.Source.fromFile("movies.txt").getLines.toList
	}

	def getMovieAvg(mId: String) = {
		getMovieJson(mId).map(x => x.map(y => getAvgAge(getActorAges(y))))
	}

	def getAvgAge(ageList: List[Int]) : Int = {
		val sum = ageList.fold(0) { (a,b) => a+b}
		println("Avg Age:" + sum/ageList.length)
		sum/ageList.length
	}

	def getMovieJson(mId: String) : Future[\/[String, Json]] = {
		val query = """{
		  "type": "/film/film",
		  "mid": """+"\""+mId+"\""+""",
		  "name": null,
		  "starring": [{
		    "actor": {
		      "type": "/people/person",
		      "name": null,
		      "mid": null,
		      "date_of_birth": null
			    }
			  }]
			}"""
		val request = constructFreebaseQuery(query)
		val res = for (exc <- Http(request OK as.String).either.left) 
			yield ("Can't connect to freebase: " + exc.getMessage)
		val resEither = res.map(\/.fromEither(_))
		val retJson = resEither.map(x => x.flatMap(y => Parse.parse(y)))
		
		retJson
	}

	def getActorAges(json: Json) : List[Int] = {
		val actorLens = jObjectPL >=> 
		jsonObjectPL("result") >=> 
		jObjectPL >=>
		jsonObjectPL("starring") >=>
		jArrayPL

		val ageLens = jObjectPL >=>
		jsonObjectPL("actor") >=>
		jObjectPL >=>
		jsonObjectPL("date_of_birth") >=>
		jStringPL

		val movieNameLens = jObjectPL >=> 
		jsonObjectPL("result") >=> 
		jObjectPL >=>
		jsonObjectPL("name") >=>
		jStringPL

		val dobList = actorLens.get(json).get.map(x=>ageLens.get(x))
		//hacky way to id movie
		println("Movie: " + movieNameLens.get(json))

		dobList.map(x => calculateAge(x.get))

	}

	//hacky function to determine age
	def calculateAge(dob: String) : Int = {
		val format = new java.text.SimpleDateFormat("yyyy-MM-dd")
		val current = format.format(new java.util.Date()).split("-").map(_.toInt)
		val dobSplit = dob.split("-").map(_.toInt)
		if ((current(1)>dobSplit(1)) && (current(2)>dobSplit(2)))
			current(0).toInt - dobSplit(0) + 1
		else 
			current(0) - dobSplit(0)
	}


}

class MovieAge{

}