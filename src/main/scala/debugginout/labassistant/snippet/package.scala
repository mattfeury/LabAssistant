package debugginout.labassistant {
  package object snippet {
    import scala.xml._

    import net.liftweb.common._
    import net.liftweb.http.SessionVar
    import net.liftweb.http.js._
      import JE.{Call, JsRaw}
      import JsCmds._
    import net.liftweb.http.SHtml._
    import net.liftweb.util.Helpers._

    import debugginout.labassistant.model._

    object pendingFunction extends SessionVar[Box[()=>JsCmd]](Empty)

    def onSubmitButtonLoginless(fn:()=>JsCmd) : (NodeSeq)=>NodeSeq = {
      case input:Elem if (input.label == "input" && input.attributes("type") == Text("submit")) ||
                         (input.label == "button") =>
        val submitClick = JsRaw("liftAjax.lift_uriSuffix = this.name + '=_'; return true;")

        ("*" #> onSubmitUnit(fn) &
         "* [onclick]" #> submitClick)(input)
    }

    def onSubmitButton(fn:()=>JsCmd) : (NodeSeq)=>NodeSeq = onSubmitButtonLoginless(verifyLogin(fn) _)

    def verifyLogin(fn:()=>JsCmd)() : JsCmd = {
      if (session.get.isDefined)
        fn()
      else {
        pendingFunction(Full(fn))

        Call("openstudy.showSignup").cmd
      }
    }

    def onEventIf(question:String, fn:(String)=>JsCmd) : (String, JsExp) = {
      val (id, jsExp) = onEvent(fn)

      (id, JsRaw(Confirm(question, jsExp).toJsCmd))
    }
  }
}
