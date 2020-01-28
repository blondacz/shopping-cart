package net.softwaria.cart

import net.softwaria.cart.Offer._
import net.softwaria.cart.ShoppingCart.Zero
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class OfferTest extends AnyFreeSpec with Matchers {


  "'Buy 1 Get 1 Free' offer for Apples " - {
    "should not apply" - {
      "when no items provided" in {
        val cart = Nil
        applyOffer(Buy1GetOneFreeOfferFoApples, cart) shouldBe Zero
      }

      "for one Apple only" in {
        val cart = Apple :: Nil
        applyOffer(Buy1GetOneFreeOfferFoApples, cart) shouldBe Zero
      }

      "for Oranges" in {
        val cart = Orange :: Nil
        applyOffer(Buy1GetOneFreeOfferFoApples, cart) shouldBe Zero
      }
    }


    val singleApplePrice = ShoppingCart.prices(Apple)

    "should apply 'Buy 1 Get 1 Free' offer on Apples" - {

      "once for 2 apples" in {
        val cart = Apple :: Apple :: Nil

        applyOffer(Buy1GetOneFreeOfferFoApples, cart) shouldBe singleApplePrice
      }

      "once for 3 apples" in {
        val cart = List.fill(3)(Apple)

        applyOffer(Buy1GetOneFreeOfferFoApples, cart) shouldBe singleApplePrice
      }

      "twice for 4 apples" in {
        val cart = List.fill(4)(Apple)

        applyOffer(Buy1GetOneFreeOfferFoApples, cart) shouldBe 2 * singleApplePrice
      }

      "twice for 5 apples" in {
        val cart = List.fill(5)(Apple)

        applyOffer(Buy1GetOneFreeOfferFoApples, cart) shouldBe 2 * singleApplePrice
      }
    }
  }

  "'Buy 1 Get 1 Free' offer for Banana " - {
    "should not apply" - {
      "when no items provided" in {
        val cart = Nil
        applyOffer(Buy1GetOneFreeOfferFoBanana, cart) shouldBe Zero
      }

      "for one Banana only" in {
        val cart = Banana :: Nil
        applyOffer(Buy1GetOneFreeOfferFoBanana, cart) shouldBe Zero
      }

      "for Oranges" in {
        val cart = Orange :: Nil
        applyOffer(Buy1GetOneFreeOfferFoBanana, cart) shouldBe Zero
      }
    }


    val singleBananaPrice = ShoppingCart.prices(Banana)

    "should apply 'Buy 1 Get 1 Free' offer on Banana" - {

      "once for 2 Bananas" in {
        val cart = Banana :: Banana :: Nil

        applyOffer(Buy1GetOneFreeOfferFoBanana, cart) shouldBe singleBananaPrice
      }

      "once for 3 Bananas" in {
        val cart = List.fill(3)(Banana)

        applyOffer(Buy1GetOneFreeOfferFoBanana, cart) shouldBe singleBananaPrice
      }

      "twice for 4 Bananas" in {
        val cart = List.fill(4)(Banana)

        applyOffer(Buy1GetOneFreeOfferFoBanana, cart) shouldBe 2 * singleBananaPrice
      }

      "twice for 5 Bananas" in {
        val cart = List.fill(5)(Banana)

        applyOffer(Buy1GetOneFreeOfferFoBanana, cart) shouldBe 2 * singleBananaPrice
      }
    }
  }


  "'3 for price of 2' offer for Bannanas" - {
    "should not apply " - {
      "when no items provided" in {
        val cart = Nil
        applyOffer(Buy3For2OfferForOranges, cart) shouldBe Zero
      }

      "for one Orange only" in {
        val cart = Orange :: Nil
        applyOffer(Buy3For2OfferForOranges, cart) shouldBe Zero
      }

      "for 2 Oranges only" in {
        val cart = Orange :: Orange :: Nil
        applyOffer(Buy3For2OfferForOranges, cart) shouldBe Zero
      }

      "for Apple" in {
        val cart = Apple :: Nil
        applyOffer(Buy3For2OfferForOranges, cart) shouldBe Zero
      }
    }


    val singleOrangePrice = ShoppingCart.prices(Orange)

    "should apply" - {
      "once for 3 oranges" in {
        val cart = List.fill(3)(Orange)

        applyOffer(Buy3For2OfferForOranges, cart) shouldBe singleOrangePrice
      }

      "once for 4 oranges" in {
        val cart = List.fill(4)(Orange)

        applyOffer(Buy3For2OfferForOranges, cart) shouldBe singleOrangePrice
      }

      "once for 5 oranges" in {
        val cart = List.fill(5)(Orange)

        applyOffer(Buy3For2OfferForOranges, cart) shouldBe singleOrangePrice
      }

      "twice for 6 oranges" in {
        val cart = List.fill(6)(Orange)

        applyOffer(Buy3For2OfferForOranges, cart) shouldBe singleOrangePrice * 2
      }

      "twice for 7 oranges" in {
        val cart = List.fill(7)(Orange)
        applyOffer(Buy3For2OfferForOranges, cart) shouldBe singleOrangePrice * 2
      }
    }
  }


  "'Cheapest Free' offer for Apples and Bananas" - {
    "should not apply " - {
      "when no items provided" in {
        val cart = Nil
        applyOffer(CheapestFreeForApplesAndBananas, cart) shouldBe Zero
      }

      "for one Apple only" in {
        val cart = Apple :: Nil
        applyOffer(CheapestFreeForApplesAndBananas, cart) shouldBe Zero
      }

      "for one Banana only" in {
        val cart = Banana :: Nil
        applyOffer(CheapestFreeForApplesAndBananas, cart) shouldBe Zero
      }
    }


    val singleBananaPrice = ShoppingCart.prices(Banana)

    "should apply" - {
      "once for Apple and Banana" in {
        val cart = Apple :: Banana :: Nil

        applyOffer(CheapestFreeForApplesAndBananas, cart) shouldBe singleBananaPrice
      }
    }
  }
}


