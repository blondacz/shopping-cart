package net.softwaria.cart

trait Offer {
  def totalDiscount: BigDecimal

  def withItem(item: Item): Offer

  def filter : (Item => Boolean)

  def execute(items: List[Item]): (Option[Offer], List[Item]) = {
    val (tc,rest) = items.foldLeft((this : Offer,Nil : List[Item])) {
      case ((ct,unused),item) => if (ct.totalDiscount <= ShoppingCart.Zero && filter(item)) ((ct.withItem(item)),unused) else (ct,item::unused)
    }
    if (tc.totalDiscount > ShoppingCart.Zero) (Some(tc), rest) else (None,rest)
  }
}

object Offer {
  val Buy1GetOneFreeOfferFoApples: Buy1GetOneFreeOffer =  Buy1GetOneFreeOffer(_ == Apple, ShoppingCart.prices(Apple),name = "B1G1 Apples")
  val Buy3For2OfferForOranges: Buy3For2Offer =  Buy3For2Offer(_ == Orange, ShoppingCart.prices(Orange), name = "B3F2 Oranges")
  val Buy1GetOneFreeOfferFoBanana :  Buy1GetOneFreeOffer  = Buy1GetOneFreeOffer(_ == Banana, ShoppingCart.prices(Banana),name = "B1G1 Banana")
  val CheapestFreeForApplesAndBananas : CheapestFree = CheapestFree(Map(Banana -> 0, Apple -> 0),ShoppingCart.prices(Banana), name = "CH3 Banana Apple")
  val allOffers: Set[Offer] = Set(Buy1GetOneFreeOfferFoApples,Buy3For2OfferForOranges, CheapestFreeForApplesAndBananas, Buy1GetOneFreeOfferFoBanana)

  val applyOffer: (Offer, ShoppingCart.Cart) => BigDecimal = (offer, cart) => cart.foldLeft(offer)((o, i) => o.withItem(i)).totalDiscount
}

case class Buy1GetOneFreeOffer(filter: Item => Boolean, discount: BigDecimal, count: Long = 0, name: String = "") extends Offer {
  override def totalDiscount: BigDecimal = count / 2 * discount
  override def withItem(item: Item): Offer = if (filter(item)) this.copy(count = count + 1) else this


}

case class Buy3For2Offer(filter: Item => Boolean, discount: BigDecimal, count: Long = 0, name: String = "") extends Offer {
  override def totalDiscount: BigDecimal = count / 3 * discount
  override def withItem(item: Item): Offer = if (filter(item)) this.copy( count = count + 1) else this
}

case class CheapestFree(counts: Map[Item,Int], discount: BigDecimal,  name: String = "") extends Offer {
  override def totalDiscount: BigDecimal = counts.minBy(_._2)._2 * discount

  override def withItem(item: Item): Offer =
    counts.get(item).map( _ + 1).map(c => this.copy(counts = counts.updated(item,c))).getOrElse(this)

  override def filter: Item => Boolean = counts.get(_).isDefined
}
