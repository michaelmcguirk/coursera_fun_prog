

type Set = Int => Boolean

val a = Set(1,2,3)
val b = Set(2,3,4)

def union(s: Set, t: Set): Set = {
  def u(i: Int): Boolean ={
    s(i) || t(i)
  }
  u
}

def singletonSet(elem: Int): Set = x => x == elem

singletonSet(2)

union(a,b)
