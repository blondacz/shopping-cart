package net.softwaria.cart

import scala.reflect.ClassTag

trait Terminator[A, B] {

  def exhausted(a: List[A], b: B): Boolean
  def invalid(a: List[A], b: B) : Boolean
  def execute(a: A, b: B): (Option[A], B)
}

object Terminator {
  def apply[A, B](implicit x: Terminator[A, B]): Terminator[A, B] = x

}


object BestOfferApplication extends App {

  implicit val xForOffers: Terminator[Offer, List[Item]] = new Terminator[Offer,List[Item]] {
    override def exhausted(a: List[Offer], b: List[Item]): Boolean = a.isEmpty || b.isEmpty

    override def invalid(a: List[Offer], b: List[Item]): Boolean = false

    override def execute(a: Offer, b: List[Item]): (Option[Offer], List[Item]) = a.execute(b)
  }

  def combination[A: ClassTag, B: ClassTag](choices: List[A], population: B)(implicit X: Terminator[A, B]): List[List[A]] = {
    def loop(choices: List[A], population: B, acc: List[A]): List[List[A]] = {
      if (X.exhausted(choices,population)) {
        List(acc)
      } else if (X.invalid(choices,population)) {
        Nil
      } else {
        choices match {
          case Nil => Nil
          case x :: xs =>
            val (choice: Option[A], newPopulation: B) = X.execute(x, population)
            loop(xs, population, acc) ::: choice.map(c => loop(x :: xs, newPopulation, c::acc)).getOrElse(Nil)
        }
      }
    }

    loop(choices, population, Nil)
  }


  private val value: List[Item] = Apple :: Apple :: Orange :: Apple :: Apple  :: Apple :: Apple:: Banana :: Banana :: Nil
  private val coms: List[List[Offer]] = combination(Offer.allOffers.toList, value)
  coms.foreach(l =>
    {
      val disc = l.map(_.totalDiscount).sum
      println(l + " - " + disc)
    })

  println( "Max:" + coms.maxByOption(_.map(_.totalDiscount).sum))

}
