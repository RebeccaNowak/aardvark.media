namespace CorrelationDrawing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Pages =

  open Aardvark.UI
  open Aardvark.UI.Primitives
  open Aardvark.Application
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.Base.Rendering
  open UtilitiesGUI

  let initialCamera = { 
          CameraController.initial with 
              view = CameraView.lookAt (V3d.III * 3.0) V3d.OOO V3d.OOI
      }


  type Action = 
      | Camera                        of CameraController.Message
      | CenterScene
      | UpdateConfig                  of DockConfig
      | Export
      | Save
      | Load
      | Clear
      | Undo
      | Redo
      | SetCullMode                   of CullMode
      | ToggleFill
      | CorrelationDrawingAppMessage  of CorrelationDrawingApp.Action
      | CorrelationDrawingMessage     of CorrelationDrawing.Action
      | SemanticAppMessage            of SemanticApp.Action
//      | KeyDown                       of key : Keys
//      | KeyUp                         of key : Keys   

  let initial   = 
    { 
        past = None
        future = None
        cameraState = initialCamera
        cullMode = CullMode.None
        fill = true
        dockConfig =
            config {
                content (
                  vertical 1.0 [
                    element { id "controls"; title "Controls"; weight 1 }
                    stack 9.0 (Some "render") [dockelement {id "render"; title "Render View"; weight 5};
                                               dockelement { id "semantics"; title "Semantics"; weight 5}]
                  ]
                )
                appName "CDPages"
                useCachedConfig false
            }
        semanticApp = SemanticApp.getInitialWithSamples
        drawingApp = CorrelationDrawingApp.initial
    }

  let update (model : Pages) (msg : Action) =
      match msg with
          | SemanticAppMessage m ->
//            match m with
//              | SemanticApp.Action.SetSemantic s ->
                {model with semanticApp = SemanticApp.update model.semanticApp m}
          | CorrelationDrawingAppMessage m ->
              {model with drawingApp = CorrelationDrawingApp.update model.drawingApp model.semanticApp.selectedSemantic m}
          | Camera m -> 
              { model with cameraState = CameraController.update model.cameraState m }

          | CenterScene -> 
              { model with cameraState = initialCamera }

          | UpdateConfig cfg ->
              { model with dockConfig = cfg; past = Some model }

          | SetCullMode mode ->
              { model with cullMode = mode; past = Some model }

          | ToggleFill ->
              { model with fill = not model.fill; past = Some model }

          | Save -> 
//                  Serialization.save model ".\drawing"
                  model
          | Load -> model
//                  Serialization.load ".\drawing"
          | Clear -> model
//                  { model with drawing = { model.drawing with annotations = PList.empty }}            

          | Undo ->
              match model.past with
                  | Some p -> { p with future = Some model; cameraState = model.cameraState }
                  | None -> model

          | Redo ->
              match model.future with
                  | Some f -> { f with past = Some model; cameraState = model.cameraState }
                  | None -> model

          | _   -> model

  let viewScene (model : MPages) =
      Sg.box (Mod.constant C4b.Green) (Mod.constant Box3d.Unit)
      |> Sg.shader {
          do! DefaultSurfaces.trafo
          do! DefaultSurfaces.vertexColor
          do! DefaultSurfaces.simpleLighting
      }
      |> Sg.cullMode model.cullMode
      |> Sg.fillMode (model.fill |> Mod.map (function true -> FillMode.Fill | false -> FillMode.Line))


  let view (model : MPages) =
    let toggleBox (str : string) (state : IMod<bool>) (toggle : 'msg) =

          let attributes = 
              amap {
                      yield attribute "type" "checkbox"
                      yield onChange (fun _ -> toggle)
                      let! check = state
                      if check then
                          yield attribute "checked" "checked"
              }

          onBoot "$('#__ID__').checkbox()" (
              div [clazz "ui small toggle checkbox"] [
                  Incremental.input (AttributeMap.ofAMap attributes)
                  label [] [text str]
              ]
          )
  
    let menu = 
      let menuItems = [div [clazz "item"]
                  [button [clazz "ui icon button"; onMouseClick (fun _ -> Save)] 
                          [i [clazz "small save icon"] [] ] |> wrapToolTip "save"];
              div [clazz "item"]
                  [button [clazz "ui icon button"; onMouseClick (fun _ -> Load)] 
                          [i [clazz "small folder outline icon"] [] ] |> wrapToolTip "load"];
              div [clazz "item"]
                  [button [clazz "ui icon button"; onMouseClick (fun _ -> Clear)]
                          [i [clazz "small file outline icon"] [] ] |> wrapToolTip "clear"];
              div [clazz "item"]
                  [button [clazz "ui icon button"; onMouseClick (fun _ -> Export)]
                          [i [clazz "small external icon"] [] ] |> wrapToolTip "export"];
              div [clazz "item"]
                  [button [clazz "ui icon button"; onMouseClick (fun _ -> Undo)] 
                          [i [clazz "small arrow left icon"] [] ] |> wrapToolTip "undo"];
              div [clazz "item"]
                  [button [clazz "ui icon button"; onMouseClick (fun _ -> Redo)] 
                          [i [clazz "small arrow right icon"] [] ] |> wrapToolTip "redo"]]
      body [style "width: 100%; height:100%; background: transparent; overflow: auto";] [
        div [style "vertical-align: middle"]
            [div [clazz "ui horizontal inverted menu";
                  style "float:middle; vertical-align: middle"]
                 menuItems
            ]
      ]
    

    let renderControl =
        CameraController.controlledControl model.cameraState Camera (Frustum.perspective 60.0 0.1 100.0 1.0 |> Mod.constant) 
                    (AttributeMap.ofList [ style "width: 100%; height:100%"; attribute "data-samples" "8" ]) 
                    (viewScene model)

    page (fun request -> 
        match Map.tryFind "page" request.queryParams with
            | Some "controls" -> 
                require Html.semui (
                    menu
                )

            | Some "render" -> // renderControl
                (CorrelationDrawingApp.view model.drawingApp model.semanticApp) 
                  |> UI.map CorrelationDrawingAppMessage

//            | Some "meta" ->
//                body [] [
//                    button [onClick (fun _ -> Undo)] [text "Undo"]
//                ]

            | Some "semantics" ->
              body [] [
                SemanticApp.viewSemantics model.semanticApp 
                  |> UI.map SemanticAppMessage
              ]

            | Some other ->
                let msg = sprintf "Unknown page: %A" other
                body [] [
                    div [style "color: white; font-size: large; background-color: red; width: 100%; height: 100%"] [text msg]
                ]  

            | None -> 
                model.dockConfig |> Mod.force |> Mod.constant |> docking [
                    style "width:100%;height:100%;"
                    onLayoutChanged UpdateConfig
                ]
    )

  let threads (model : Pages) = 
      CameraController.threads model.cameraState |> ThreadPool.map Camera

  let app =                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
      {
          unpersist = Unpersist.instance     
          threads   = threads 
          initial = initial
          update = update 
          view = view
      }
