namespace CorrelationDrawing

open System
open Aardvark.Base
open Aardvark.Base.Geometry
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.UI.Primitives
open UI.Composed
open Aardvark.Rendering.Text

open Aardvark.SceneGraph.SgPrimitives
open Aardvark.SceneGraph.FShadeSceneGraph
open UtilitiesGUI    

module Serialization =
    open MBrace.FsPickler
    open System.IO
    let binarySerializer = FsPickler.CreateBinarySerializer()
    
    let save (model : CorrelationAppModel) path = 
        let arr = binarySerializer.Pickle model
        File.WriteAllBytes(path, arr);

    let load path : CorrelationAppModel = 
        let arr = File.ReadAllBytes(path);
        let app = binarySerializer.UnPickle arr
        app

    let writeToFile path (contents : string) =
        System.IO.File.WriteAllText(path, contents)

module CorrelationDrawingApp = 
    open Newtonsoft.Json
            
    type Action =
        | CameraMessage             of ArcBallController.Message
        | DrawingMessage            of CorrelationDrawing.Action
        | DrawingSemanticMessage    of CorrelationDrawing.Action
        | AnnotationMessage         of CorrelationDrawing.Action
        | KeyDown                   of key : Keys
        | KeyUp                     of key : Keys      
        | Export
        | Save
        | Load
        | Clear
        | Undo
        | Redo
                       
    let stash (model : CorrelationAppModel) =
        { model with history = Some model; future = None }

    let clearUndoRedo (model : CorrelationAppModel) =
        { model with history = None; future = None }

    let update (model : CorrelationAppModel) (act : Action) =
        match act, model.drawing.draw with
            | CameraMessage m, false -> 
                    { model with camera = ArcBallController.update model.camera m }      
            | DrawingMessage m, _ ->
                    { model with drawing = CorrelationDrawing.update model.drawing m }      
            | DrawingSemanticMessage m, _ ->
                {model with drawing = CorrelationDrawing.update model.drawing m}          
            | AnnotationMessage m, _ ->
                {model with drawing = CorrelationDrawing.update model.drawing m}          
            | Save, _ -> 
                    Serialization.save model ".\drawing"
                    model
            | Load, _ -> 
                    Serialization.load ".\drawing"
            | Clear,_ ->
                    { model with drawing = { model.drawing with annotations = PList.empty }}            
            | Undo, _ -> 
                match model.history with
                    | Some h -> { h with future = Some model }
                    | None -> model
            | Redo, _ ->
                match model.future with
                    | Some f -> f
                    | None -> model
            | KeyDown k, _ -> 
                    let d = CorrelationDrawing.update model.drawing (CorrelationDrawing.Action.KeyDown k)
                    { model with drawing = d }
            | KeyUp k, _ -> 
                    let d = CorrelationDrawing.update model.drawing (CorrelationDrawing.Action.KeyUp k)
                    { model with drawing = d }
            | _ -> model
                       
    let myCss = [
                  { kind = Stylesheet; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
                  { kind = Stylesheet; name = "semui-overrides"; url = "semui-overrides.css" }
                  { kind = Script; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
                ]
    
    let view (model : MCorrelationAppModel) =
        let frustum =
            Mod.constant (Frustum.perspective 60.0 0.1 100.0 1.0)

        let menu = 
          let foo = [div [clazz "item"]
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
                              [i [clazz "small arrow right icon"] [] ] |> wrapToolTip "redo"];
                  div [clazz "item"]
                      [button [clazz "ui icon button"; 
                                onMouseClick (fun _ -> Action.DrawingSemanticMessage CorrelationDrawing.AddSemantic)] 
                              [i [clazz "small plus icon"] [] ]]]

          div [style "vertical-align: middle"] [
          // div [style "width:100%; height: 10%; vertical-align: middle"] [
            div [clazz "ui horizontal inverted menu";style "width:100%; height: 10%; float:middle; vertical-align: middle"]
                (List.append foo (List.map (fun x -> x |> UI.map DrawingMessage) 
                                           (CorrelationDrawing.UI.viewAnnotationTools model.drawing)))]
                 
        require (myCss) (
            body [clazz "ui"; style "background: #1B1C1E;position:fixed;width:100%"] [
              menu
              div [] [
                ArcBallController.controlledControl model.camera CameraMessage frustum
                    (AttributeMap.ofList [
                                onKeyDown (KeyDown)
                                onKeyUp (KeyUp)
                                attribute "style" "width:70%; height: 100%; float: left;"]
                    )
                    (
                        CorrelationDrawing.Sg.view model.drawing model.camera.view
                            |> Sg.map DrawingMessage
                            |> Sg.fillMode (model.rendering.fillMode)
                                    
                            |> Sg.cullMode (model.rendering.cullMode)                                                                    
                    )                                        
              ]
            
              div [clazz "scrolling content"; style "width:30%; height: 100%; float: right; overflow-y: scroll"] [
                  CorrelationDrawing.UI.viewAnnotations model.drawing  |> UI.map AnnotationMessage   
                  CorrelationDrawing.UI.viewSemantics model.drawing |> UI.map DrawingSemanticMessage

            ]
        ]
//          ]
        ) 


    let initial : CorrelationAppModel =
        {
            camera           = { ArcBallController.initial with view = CameraView.lookAt (23.0 * V3d.OIO) V3d.Zero V3d.OOI}
            rendering        = RenderingPars.initial
           
            drawing = 
               CorrelationDrawing.initial 
                |> (CorrelationDrawing.insertSemantic (Semantic.initialHorizon (Guid.NewGuid().ToString())))
                |> (CorrelationDrawing.insertSemantic (Semantic.initialGrainSize (Guid.NewGuid().ToString())))
                |> (CorrelationDrawing.insertSemantic (Semantic.initialCrossbed (Guid.NewGuid().ToString())))
                      

           

            history = None
            future = None
        }

    let app : App<CorrelationAppModel,MCorrelationAppModel,Action> =
        {
            unpersist = Unpersist.instance
            threads = fun model -> ArcBallController.threads model.camera |> ThreadPool.map CameraMessage
            initial = initial
            update = update
            view = view
        }

    let start () = App.start app

