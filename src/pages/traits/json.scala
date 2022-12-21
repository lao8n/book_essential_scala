sealed trait Json {
    def print: String = {
        def listToJson(jsonList : JsonList): String = 
            jsonList match {
                case JsonListEnd => ""
                case JsonListElement(json, jsonList) => json.print + listToJson(jsonList) 
            }
        def mapToJson(jsonMap: JsonMap): String = 
            jsonMap match {
                case JsonMapEnd => ""
                case JsonMapElement(key, value, jsonMap) => key.print + v.print + ", " + mapToJson(jsonMap)
            }

        this match {
            case JsonString(string) => string
            case JsonNumber(number) => number.toString()
            case JsonDouble(double) => double.toString()
            case JsonBoolean(boolean) => boolean.toString()
            case jl : JsonListElement =>  "[" + listToJson(jl) + "]"
            case JsonListEnd => "[]"
            case jm : JsonMapElement => "{" + mapToJson(jm) + "}"
            case JsonMapEnd => "{}"
        }
    }
}
final case class JsonString(string: string) extends Json
final case class JsonNumber(number: Int) extends Json
final case class JsonDouble(double: Double) extends Json
final case class JsonBoolean(boolean: Boolean) extends Json

sealed trait JsonList extends Json 
case object JsonListEnd extends JsonList
final case class JsonListElement(json : Json, jsonList : JsonList) extends JsonList

sealed trait JsonMap extends Json
case object JsonMapEnd extends JsonMap
final case class JsonMapElement(key: JsonString, value: Json, jsonMap: JsonMap)

object Main extends App {
    JsonListElement(JsonString("a string"), 
        JsonListElement(JsonDouble(1.0), 
            JsonListElement(JsonBoolean(true), 
                JsonListEnd))).print
}