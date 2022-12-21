object Options {
    def addOptions(option1: Option[Int], option2: Option[Int]) =
        for {
            o1 <- option1
            o2 <- option2
        } yield o1 + o2

    def addOptions(option1: Option[Int], option2: Option[Int]) = 
        option1.flatMap(o1 => option2.map(o2 => o1 + o2))

    def addOptions(option1: Option[Int], option2: Option[Int], option3: Option[Int]) = 
        for {
            o1 <- option1 
            o2 <- option2
            o3 <- option3
        } yield o1 + o2 + o3
    
    def addOptions(option1: Option[Int], option2: Option[Int], option3: Option[Int]) = 
        option1.flatMap(o1 => o2.flatMap(o2 => o3.map(o3 => o1 + o2 + o3)))

    def calculator(operand1: String, operator: String, operand2: String): Unit = {
        val result = for {
            a   <- readInt(operand1)
            b   <- readInt(operand2)
            ans <- operator match {
                    case "+" => Some(a + b)
                    case "-" => Some(a - b)
                    case "*" => Some(a * b)
                    case "/" => divide(a, b)
                    case _   => None
                }
        } yield ans

        result match {
            case Some(number) => println(s"The answer is $number!")
            case None         => println(s"Error calculating $operand1 $operator $operand2")
        }
    }
}