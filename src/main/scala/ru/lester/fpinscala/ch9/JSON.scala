package ru.lester.fpinscala.ch9

/**
 * User: lester
 */

sealed trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  //TODO: Align with the latest reference Parser implementation
  /*
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P.{string => _, _}
    implicit def tok(s: String) = token(P.string(s))

    val spaces = char(' ').many.slice
    val nul = "null".r map (_ => JNull)
    val str = """[a-zA-Z]+""".r map JString
    val number = """\d+""".r map (s => JNumber(s.toDouble))
    val bool = "true" | "false" map (s => JBool(s.toBoolean))
    val literal = str | number | bool | nul
    val array = "[".r product(literal).product("]".r) map (l => JArray(l.toIndexedSeq))
  }
  */
}
