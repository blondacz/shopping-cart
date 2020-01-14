package net.softwaria.cart

import net.softwaria.cart.ShoppingCart.totalCostWithoutOffers
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ShoppingCartTest extends AnyFreeSpec with Matchers with OptionValues {

  "shopping cart - total cost without offers" - {
    "with no items has total cost of 0" in {
      totalCostWithoutOffers(Nil).value shouldBe BigDecimal("0")
    }

    "with one apple has total cost of 60p " in {
      val cart = List(Apple)

      totalCostWithoutOffers(cart).value shouldBe BigDecimal("0.60")
    }

    "with one orange has total cost of 25p " in {
      val cart = List(Orange)

      totalCostWithoutOffers(cart).value shouldBe BigDecimal("0.25")
    }

    "with two oranges has total cost of 50p " in {
      val cart = List(Orange, Orange)

      totalCostWithoutOffers(cart).value shouldBe BigDecimal("0.50")
    }

    "with two apples has total cost of £1.20 " in {
      val cart = List(Apple, Apple)

      totalCostWithoutOffers(cart).value shouldBe BigDecimal("1.20")
    }

    "with Apple, Apple, Orange, Apple has total cost of £2.05 " in {
      val cart = List(Apple, Apple, Orange, Apple)

      totalCostWithoutOffers(cart).value shouldBe BigDecimal("2.05")
    }
  }

  "shopping cart - total cost  (with offers)" - {
    "should calculate total without offers when none applicable" in {
      val cart = List(Apple, Orange, Orange)

      ShoppingCart.totalCost(cart).value shouldBe BigDecimal("1.10")
    }

    "should calculate total with 'Buy 1 Get One Free' and 'Buy 3 for 2' offers applied" in {
      val cart = List(Apple, Apple, Orange, Orange, Orange)

      ShoppingCart.totalCost(cart).value shouldBe BigDecimal("1.10")
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






