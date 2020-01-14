package net.softwaria.cart

import net.softwaria.cart.ShoppingCart.totalCost
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ShoppingCartTest extends AnyFreeSpec with Matchers with OptionValues {

  "shopping cart" - {
    "with no items has total cost of 0" in {
      totalCost(Nil).value shouldBe BigDecimal("0")
    }

    "with one apple has total cost of 60p " in {
      val cart = List(Apple)

      totalCost(cart).value shouldBe BigDecimal("0.60")
    }

    "with one orange has total cost of 25p " in {
      val cart = List(Orange)

      totalCost(cart).value shouldBe BigDecimal("0.25")
    }

    "with two oranges has total cost of 50p " in {
      val cart = List(Orange, Orange)

      totalCost(cart).value shouldBe BigDecimal("0.50")
    }

    "with two apples has total cost of £1.20 " in {
      val cart = List(Apple, Apple)

      totalCost(cart).value shouldBe BigDecimal("1.20")
    }

    "with Apple, Apple, Orange, Apple has total cost of £2.05 " in {
      val cart = List(Apple, Apple, Orange, Apple)

      totalCost(cart).value shouldBe BigDecimal("2.05")
    }
  }

  "price list" - {
    "contains price for apple" in {
      ShoppingCart.prices.get(Apple).value shouldBe BigDecimal("0.60")
    }

    "contains price for orange" in {
      ShoppingCart.prices.get(Orange).value shouldBe BigDecimal("0.25")
    }
  }
}




