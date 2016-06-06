import org.scalatest.FlatSpec

class CityMapTest extends FlatSpec {

  "Given \"a-b,b-c,c-a\"" should "create the graph" in {

    val cityMap = new CityMap("a-b,b-c,c-a")
    assert(cityMap.graph("a") === List("b"))
    assert(cityMap.graph("b") === List("c"))
    assert(cityMap.graph("c") === List("a"))

  }

  "Given \"a-b, a-c, a-d\"" should "create the graph" in {

    val cityMap = new CityMap("a-b,a-c,a-d")
    assert(cityMap.graph("a") === List("b","c","d"))
    assert(cityMap.graph.size === 1)

  }


  "Given \"a-b,b-c,c-a\", isJourneyPossible" should " return true" in {

    val cityMap = new CityMap("a-b,b-c,c-a")
    assert(cityMap.isJourneyPossible("a","c"))

  }


  "Given \"a-b,a-a,c-a\", isJourneyPossible" should " return false" in {

    val cityMap = new CityMap("a-b,a-a,c-a")
    assert(!cityMap.isJourneyPossible("a","c"))

  }

  "Given \"a-b,a-c,a-d,d-f,d-e\", isJourneyPossible" should " return true for a->e, a-f, and false for b->e and c->e and c -> f" in {

    val cityMap = new CityMap("a-b,a-c,a-d,d-f,d-e")
    assert(cityMap.isJourneyPossible("a","e"))
    assert(cityMap.isJourneyPossible("a","f"))

    assert(!cityMap.isJourneyPossible("b","e"))
    assert(!cityMap.isJourneyPossible("c","e"))
    assert(!cityMap.isJourneyPossible("c","f"))

  }


  "Given \"a-b,b-c,c-d,d-e,f-e\" " should "return true for a->b, a->c, a->d,a->e and false for a->f" in {

    val cityMap = new CityMap("a-b,b-c,c-d,d-e,f-e")
    assert(cityMap.isJourneyPossible("a","b"))
    assert(cityMap.isJourneyPossible("a","c"))
    assert(cityMap.isJourneyPossible("a","d"))
    assert(cityMap.isJourneyPossible("a","e"))
    assert(!cityMap.isJourneyPossible("a","f"))


  }

  "Given bad input \"ab,ac,ad,df,de\"" should "throw RuntimeException" in {

    intercept[RuntimeException] {
       new CityMap("ab,ac,ad,df,de")
    }
  }







}
