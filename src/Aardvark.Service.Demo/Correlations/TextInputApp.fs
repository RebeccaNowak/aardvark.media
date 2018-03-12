namespace CorrelationDrawing


open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open CorrelationDrawing.UtilitiesGUI

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TextInput =
  open Aardvark.Base
  open Aardvark.UI

  type Action =
    | ChangeText of string
    | Enable
    | Disable
    | ChangeBgColor of C4b

  let init =  {
    text = ""
    disabled = false
    bgColor = C4b.White
    size = None
  }

  let update (model : TextInput) (action : Action) =
    match action with
      | ChangeText str -> {model with text = str}
      | Enable -> {model with disabled = false}
      | Disable -> {model with disabled = true}
      | ChangeBgColor c -> {model with bgColor = c}

  let view' (model : MTextInput) : DomNode<Action> = 
    let attr1 =
      amap {
        yield attribute "type" "text"
        yield onChange (fun str -> ChangeText str)
        //let optS = Mod.force(model.size)
        //if optS.IsSome then yield attribute "size" (sprintf "%i" optS.Value)
      }

    let attributes =
      amap {
        //let! optS = model.size
        //if optS.IsSome then yield attribute "size" (sprintf "%i" optS.Value)
        let! txt = model.text
        yield attribute "value" txt
      }
    Incremental.input (AttributeMap.ofAMap (AMap.union attr1 attributes))
   
  let app  = {
    unpersist = Unpersist.instance
    threads = fun _ -> ThreadPool.empty
    initial = init
    update = update
    view = view'
  }

  let start () = App.start app
