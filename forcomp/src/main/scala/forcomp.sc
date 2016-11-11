// ("b",2)
for{
  rest <- List(List())
  num <- 0 to 2
} yield if (num == 0) rest else ("b", num) :: rest
// 0 -> List(List())
// 1 -> List(("b",1))
// 2 -> List(("b",2))
// -> List(List(), List(("b",1)), List(("b",2)))

// ("a",2)
for{
  rest <- List(List(), List(("b",1)), List(("b",2)))
  num <- 0 to 2
} yield if (num == 0) rest else ("a", num) :: rest
// 0 -> List(List(), List(("b",1)), List(("b",2)))
// 1 -> List(List(("a",1)), List(("a",1),("b",1)), List(("a",1),("b",2)))
// 2 -> List(List(("a",2)), List(("a",2),("b",1)), List(("a",1),("b",2)))
// -> List(List(), List(("b",1)), List(("b",2)),List(("a",1)), List(("a",1),("b",1)), List(("a",1),("b",2)),List(("a",2)), List(("a",2),("b",1)), List(("a",1),("b",2)))

