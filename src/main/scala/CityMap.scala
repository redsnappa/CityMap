
/**
 * @param roadsAndLocations A String specification of the
 * connections of roads and locations. A comma-separated list
 * of roads between locations defined as * [startLocation]-[endLocation] e.g.
 *
 * "a-b,b-a" represents two locations, where travel
 * is possible in both directions
 * "a-b,b-c,c-a" represents a triangular one-way road travelling * from a, through b and c, and finally back to a. */

 class CityMap(roadsAndLocations:String) {



  val graph:Map[String,List[String]] = createGraph(roadsAndLocations)

  def isJourneyPossible(startLocation:String, destinationLocation:String):Boolean = {

   def innerJP(visited:List[String], remaining:List[String], st:String):Boolean = {

    if(graph.contains(st)){
     val unexplored = graph(st).filter(s => !(visited contains s));
     unexplored.contains(destinationLocation) || innerJP(st :: visited, unexplored.tail ::: remaining, unexplored.head)

    }else if(!(remaining isEmpty)){
      innerJP(st::visited, remaining.tail, remaining.head)
    }else{
     false
    }
   }

   innerJP(List(startLocation), List() , startLocation)
  }


 /**
  * This is used to turn a string with the format src-dest,src-dest,src-dest into an adjacency list
  * @param roads
  * @return
  */
 private def createGraph(roads:String):Map[String,List[String]] = {

  val srcDestinationList:Array[(String, String)] = roads.split(",").map((str:String) => { val array = str.split("-");  (array(0),array(1)) });
  return srcDestinationList.groupBy( _._1 ) map { case (k,v) => (k,v.map(_._2).toList)}

 }


 override def toString():String = {

  graph map { case (key,value) =>  key + "->" + value.mkString("[",",","]") } mkString(", ")

 }

}

