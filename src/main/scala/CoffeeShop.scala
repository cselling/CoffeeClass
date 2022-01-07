class CoffeeShop {
  case class MenuItem(name: String, foodType: String, price: Double){}
  val name = "Selling Coffee"
  var menu = Array[MenuItem](MenuItem("Coffee", "Drink", 10.95),MenuItem("Espresso", "Drink", 6.95),MenuItem("Latte", "Drink", 10.95),MenuItem("Mocha", "Drink", 10.95),MenuItem("Biscotti", "Food", 2.95),MenuItem("Blueberry Muffin", "Food", 3.95),MenuItem("Cranberry Scone", "Food", 3.95),MenuItem("Pancakes", "Food", 2.95),MenuItem("Waffles", "Food", 2.95),MenuItem("Chocolate Chip Muffin", "Food", 3.95))
  var orders = Array[MenuItem]()

  //addOrder: adds the name of the item to the end of the orders array if it exists on the menu. Otherwise, return "This item is currently unavailable!"
  def addOrder(newOrder: String): String = {
    val order = menu.find(order => order.name == newOrder)
    if (order == null){
      return "This item is currently unavailable!"
    } else {
      orders  = orders :+ order.get
      return s"One ${order.get.name} coming up!"
    }
  }
  //fulfillOrder: if the orders array is not empty, return "The {item} is ready!". If the orders array is empty, return "All orders have been fulfilled!"
  def fulfillOrder(): String ={
    if(orders.length == 0){
      return "All orders have been fulfilled"
    } else{
      val result = orders(0).name
      orders = orders.takeRight(orders.length-1)
      return s"The $result is ready!"
    }
  }
  //listOrders: returns the list of orders taken, otherwise, an empty array.
  def listOrders() : Array[String]={
    if (orders.length == 0){
      return new Array(0)
    } else {
      return orders.map(item => item.name)
    }
  }
  //dueAmount: returns the total amount due for the orders taken.
  def dueAmount(): Double = {
    val prices = orders.map(item => item.price)
    return prices.sum
  }
  //cheapestItem: returns the name of the cheapest item on the menu.
  def cheapestItem(): MenuItem = {
    var result: MenuItem = MenuItem("Test", "Food", 1000)
    for( x <- menu){
      if (x.price < result.price){
        result = x
      }
    }
    return result
  }
  //drinksOnly: returns only the item names of type drink from the menu.
  def drinksOnly(): Array[String] = {
    val drinks = menu.filter(item => item.foodType == "Drink")
    return drinks.map(item => item.name)
  }
  //foodOnly: returns only the item names of type food from the menu.
  def foodOnly(): Array[String] = {
    val foods = menu.filter(item => item.foodType == "Food")
    return foods.map(item => item.name)
  }
}

object testMain {
  def main(args: Array[String]): Unit ={
    val shop = new CoffeeShop()
    shop.addOrder("Coffee")
    shop.addOrder("Espresso")
    shop.addOrder("Pancakes")
    val today = shop.listOrders()
    print("Current Orders:")
    for(x <- today){
      print(s" $x")
    }
    print("\n")

    println(shop.dueAmount())
    println(shop.cheapestItem())
    println(shop.fulfillOrder())
    val today2 = shop.listOrders()
    print("Current Orders:")
    for(x <- today2){
      print(s" $x")
    }
    print("\n")
    val drinks = shop.drinksOnly()
    print("Drinks Only:")
    for(x <- drinks){
      print(s" $x")
    }

    val foods = shop.foodOnly()
    print("\nFoods Only:")
    for(x <- foods){
      print(s" $x")
    }
  }
}