object EqualAttempt {
    def apply[A](first: A, second: A)(implicit eq: Equal[A]): Boolean = 
        eq.equal(first, second)
}

case class Person(name: String, email: String)

object EmailEqual {
    implicit object emailEqual extends Equal[Person] = {
        def equal(first: Person, second: Person): Boolean = {
            first.email == second.email
        }
    }
}

object NameEmailEqual {
    implicit object nameEqual extends Equal[Person] = {
        def equal(first: Person, second: Person): Boolean = {
            first.name == second.name && first.email == second.email
        }
    }
}

object Example {
    def emailEqual = {
        import EmailEqual._
        Print(EqualAttempt(Person("Nicholas", "hellolao8n@gmail.com"), Person("Nicholas", "hellolao8n@gmail.com")))
    }

    def emailNameEqual = {
        import NameEmailEqual._
        Print(EqualAttempt(Person("Nicholas", "hellolao8n@gmail.com"), Person("Nicholas", "hellolao8n@gmail.com")))
    }

    emailEqual()
    emailNameEqual()
}

// import NameAndEmailImplicit._
// Equal[Person].equal(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com"))
// object Equal {
//   def apply[A](implicit instance: Equal[A]): Equal[A] =
//     instance
// }


