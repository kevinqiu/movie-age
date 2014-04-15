import dispatch._
import Defaults._
import argonaut._, Argonaut._
import scalaz.\/
import scalaz.std.option._
import scalaz.syntax.validation._ 

object MovieAge {
	val apiKeySouce = io.Source.fromFile(".api_key")
	val apiKey = apiKeySouce.mkString
	apiKeySouce.close()

	def main(args: Array[String]) {
		getCurrentMovies.map(getMovieAvg(_))
	}

	//wrapper to send query to freebase
	def constructFreebaseQuery(query: String) : dispatch.Req = {
		val freebase = host("www.googleapis.com").secure / "freebase" / "v1" / "mqlread"
		freebase <<? Map("query" -> query) <<? Map("key" -> apiKey)
	}

	//currently reads movies from file, TODO: get current top box office movies
	//from some source
	def getCurrentMovies() : List[String] = {
		io.Source.fromFile("movies.txt").getLines.toList
	}

	//compositional function that has side effect of printing age out
	def getMovieAvg(mId: String) = {
		getMovieJson(mId).map(json => 
			json.fold(
				error => {
					println("Unable to retrieve json")
					},
					data => {
						val movie = getMovieName(data).get
						getActors(data).map(
							actors => {
								val avg = getAvgAge(actors.map(actor => getDob(actor).map(
									dob => calculateAge(dob))).flatten)
								println("Average age of actors in " + movie + " is: " + avg)
								}).getOrElse(println("actors for" + movie + "could not be found"))
					}))	
	}

	def getAvgAge(ageList: List[Int]) : Int = {
		val sum = ageList.fold(0) { (a,b) => a+b}
		sum/ageList.length
	}

	//retrieves the movie data from freebase
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
		//convert to scalaz either so flatmap can be used with argonaut 
		//parser which also uses scalaz \/
		val resEither = res.map(\/.fromEither(_))
		val retJson = resEither.map(x => x.flatMap(y => Parse.parse(y)))
		
		retJson
	}

	//parse json to retrieve list of actors
	def getActors(json: Json) : Option[JsonArray] = {
		val actorLens = jObjectPL >=> 
		jsonObjectPL("result") >=> 
		jObjectPL >=>
		jsonObjectPL("starring") >=>
		jArrayPL

		actorLens.get(json)
	}

	//parse json to retrieve dob from actor json
	def getDob(json: Json) : Option[String] = {
		val ageLens = jObjectPL >=>
		jsonObjectPL("actor") >=>
		jObjectPL >=>
		jsonObjectPL("date_of_birth") >=>
		jStringPL

		ageLens.get(json)
	}

	//parse json to retrieve name from movie json
	def getMovieName(json : Json) : Option[String] = {
		val movieNameLens = jObjectPL >=> 
		jsonObjectPL("result") >=> 
		jObjectPL >=>
		jsonObjectPL("name") >=>
		jStringPL

		movieNameLens.get(json)
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