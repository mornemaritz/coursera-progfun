val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern", "South Africa" -> "Pretoria")
val cap1 = capitalOfCountry withDefaultValue "unknown"

cap1("e")
capitalOfCountry.get("South Africa")
capitalOfCountry.get("Andorra")

def showCapital(country: String) = capitalOfCountry.get(country) match {
  case Some(capital) => capital
  case None => "missing data"
}

showCapital("US")

val fruit = List("apple", "pear", "orange", "pineapple")
fruit groupBy(_.head)



