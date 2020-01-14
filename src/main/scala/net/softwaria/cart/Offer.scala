package net.softwaria.cart

trait Offer {
  def totalDiscount: BigDecimal

  def withItem(item: Item): Offer
}

object Offer {
  val Buy1GetOneFreeOfferFoApples: Buy1GetOneFreeOffer = new Buy1GetOneFreeOffer(_ == Apple, ShoppingCart.prices(Apple))
  val Buy3For2OfferForOranges: Buy3For2Offer = new Buy3For2Offer(_ == Orange, ShoppingCart.prices(Orange))
  val allOffers: List[Offer] = List(Buy1GetOneFreeOfferFoApples,Buy3For2OfferForOranges)

  val applyOffer: (Offer, ShoppingCart.Cart) => BigDecimal = (offer, cart) => cart.foldLeft(offer)((o, i) => o.withItem(i)).totalDiscount
}

class Buy1GetOneFreeOffer(filter: Item => Boolean, discount: BigDecimal, val count: Long = 0) extends Offer {
  override def totalDiscount: BigDecimal = count / 2 * discount

  override def withItem(item: Item): Offer = if (filter(item)) new Buy1GetOneFreeOffer(filter, discount, count + 1) else this
}

class Buy3For2Offer(filter: Item => Boolean, discount: BigDecimal, val count: Long = 0) extends Offer {
  override def totalDiscount: BigDecimal = count / 3 * discount

  override def withItem(item: Item): Offer = if (filter(item)) new Buy3For2Offer(filter, discount, count + 1) else this
}
