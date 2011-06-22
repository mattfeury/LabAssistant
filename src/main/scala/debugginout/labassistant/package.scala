package debugginout {
  import java.util.Date

  import net.liftweb.common.Box
  import net.liftweb.http.Req
  import net.liftweb.util.Helpers._

  import org.bson.types.ObjectId

  package object labassistant {
    implicit def objectId2String(id:ObjectId) : String = id.toString

    def printRed(thing:Any) = {
      println("\u001b[0m\u001b[31m" + thing + "\u001b[0m")
    }
    def printGreen(thing:Any) = {
      println("\u001b[0m\u001b[32m" + thing + "\u001b[0m")
    }
    def printYellow(thing:Any) = {
      println("\u001b[0m\u001b[33m" + thing + "\u001b[0m")
    }
    def printBox[T](thing:Box[T]) = {
      import net.liftweb.common.{Full, Failure, Empty}
      thing match {
        case Full(_) =>
          printGreen(thing)
        case Failure(_, _, _) =>
          printYellow(thing)
        case Empty =>
          printRed(thing)
      }
    }

    object Inflector {
      val conversions =
        List(("^([Pp])erson$".r -> "$1eople"),
             ("^is$".r -> "are"),
             ("^has$".r -> "have"),
             ("^(.*)y$".r  -> "$1ies"),
             ("^(.*)$".r   -> "$1s"))

      // Takes the given word and makes it plural.
      def pluralize(word:String) : String = {
        for ((matcher, replacement) <-
                conversions.find((conversion) => conversion._1.findFirstIn(word).isDefined))
          return matcher.replaceFirstIn(word, replacement)

        return word
      }
    }

    class StringWithInflections(string:String) {
      // Makes this string plural (assuming this string is a word).
      def pluralize = {
        Inflector.pluralize(string)
      }

      // Makes this string plural only if the given number requires it.
      def maybePluralize(number:Long) : String = {
        (number != 1) ? pluralize | string
      }
    }

    implicit def string2stringWithInflections(string:String) : StringWithInflections = {
      new StringWithInflections(string)
    }

    // Pluralizes the given word if necessitated by the given number.
    def pluralize(number:Long, word:String) : String = {
      if (number == 1)
        number + " " + word
      else
        number + " " + word.pluralize
    }

  }
}
