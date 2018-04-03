﻿namespace CorrelationDrawing


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

  let view' (styleStr : IMod<string>) (model : MTextInput): DomNode<Action> = 
    let attr1 =
      amap {
        yield attribute "type" "text"
        let! st = styleStr
        yield style st
        yield onChange (fun str -> ChangeText str)
      }

    let attributes =
      amap {
        let! txt = model.text
        yield attribute "value" txt
      }
    div [clazz "ui icon input"] [(Incremental.input (AttributeMap.ofAMap (AMap.union attr1 attributes)))]
    //style "height: 1.4285em"

  let view'' (styleStr : string) (model : MTextInput): DomNode<Action> = 
    let attr1 =
      amap {
        yield attribute "type" "text"
        //let! st = styleStr
        yield style styleStr
        yield onChange (fun str -> ChangeText str)
      }

    let attributes =
      amap {
        let! txt = model.text
        yield attribute "value" txt
      }
    div [clazz "ui icon input"] [(Incremental.input (AttributeMap.ofAMap (AMap.union attr1 attributes)))]

  let view (model : MTextInput) : DomNode<Action> =
    view' (Mod.constant "") model  
     
  let app  = {
    unpersist = Unpersist.instance
    threads = fun _ -> ThreadPool.empty
    initial = init
    update = update
    view = view
  }

  let start () = App.start app
