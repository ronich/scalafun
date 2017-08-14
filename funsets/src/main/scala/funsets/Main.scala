package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  printSet(diff(intersect((x: Int) => x > 500, (x: Int) => x < 506), singletonSet(504)))
}
