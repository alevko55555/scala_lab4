abstract class Summator[T] {
    def sum(lhs: T, rhs: T) : T
}
object Summator {
   def sum[T: Summator](lhs: T, rhs: T) : T = {
       implicitly[Summator[T]].sum(lhs, rhs)
   }

   implicit val int2Summator: Summator[Int] = new Summator[Int] {
       override def sum(lhs: Int, rhs: Int) : Int = {
            return lhs + rhs
       }
   }

   implicit val bool2Summator: Summator[Boolean] = new Summator[Boolean] {
       override def sum(lhs: Boolean, rhs: Boolean) : Boolean = {
            return lhs | rhs
       }
   }
}

abstract class And[T] {
    def and(lhs: T, rhs: T) : T
}

object And {
    def and[T: And](lhs: T, rhs: T) : T = {
        implicitly[And[T]].and(lhs, rhs)
    }
    implicit val bool2Summator: And[Boolean] = new And[Boolean] {
       override def and(lhs: Boolean, rhs: Boolean) : Boolean = {
            return lhs && rhs
       }
   }
}

abstract class Or[T] {
    def or(lhs: T, rhs: T) : T
}

object Or {
    def or[T: Or](lhs: T, rhs: T) : T = {
        implicitly[Or[T]].or(lhs, rhs)
    }
    implicit val bool2Summator: Or[Boolean] = new Or[Boolean] {
       override def or(lhs: Boolean, rhs: Boolean) : Boolean = {
            return lhs || rhs
       }
   }
}


class SuperNumber[T](num: T){
    var value = num

    def +(r: SuperNumber[T])(implicit summator: Summator[T]) : SuperNumber[T] = {
        var b = summator.sum(value, r.value)
        return new SuperNumber(b)
    }

    def ||(r: SuperNumber[T])(implicit or: Or[T]) : SuperNumber[T] = {
        val b = or.or(r.value, value)
        return new SuperNumber(b)
    }

    def &&(r: SuperNumber[T])(implicit a: And[T]) : SuperNumber[T] = {
        val b = a.and(r.value, value)
        return new SuperNumber(b)
    }

    def getStr():String = {
        return s"${value}"
    }
}

var a = new SuperNumber(2)
var b = new SuperNumber(3)
println((a + b).getStr())
//var c = (a || b) error
var a1 = new SuperNumber(false)
var b1 = new SuperNumber(true)
println((a1 + b1).getStr())
println((a1 || b1).getStr())
println((a1 && b1).getStr())