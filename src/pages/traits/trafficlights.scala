// no external data so pattern matching inside class is best

// polymorphism
sealed trait TrafficLight {
    def next: TrafficLight
}
final case object Red extends TrafficLight {
    val next : TrafficLight = Green
}
final case object Green extends TrafficLight {
    val next : TrafficLight = Yellow
}
final case object Yellow extends TrafficLight {
    val next : TrafficLight = Red
}

// pattern matching inside class
sealed trait TrafficLightPM {
    def next: TrafficLight = 
        this match {
            case Red => Green
            case Green => Yellow
            case Yellow => Red
        }
}
case object Red extends TrafficLight
case object Green extends TrafficLight
case object Yellow extends TrafficLight

// pattern matching outside object
object TrafficLightPM {
    def next(current: TrafficLight): TrafficLight = 
        current match {
            case Red => Green
            case Green => Yellow
            case Yellow => Red
        }
}
case object Red extends TrafficLight
case object Green extends TrafficLight
case object Yellow extends TrafficLight