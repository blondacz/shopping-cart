package net.softwaria.cart

object ShoppingCart {
  type Cart = List[Item]
  type Prices = Map[String, BigDecimal]
  final val Zero = BigDecimal(0)

  val prices: Map[Item, BigDecimal] = Map(
    Apple -> BigDecimal("0.60"),
    Orange -> BigDecimal("0.25"))

  val totalCost : Cart => Option[BigDecimal] = items => items.foldLeft(Option(Zero))((tc,i) => tc.flatMap(c => prices.get(i).map(_ + c)))
}
