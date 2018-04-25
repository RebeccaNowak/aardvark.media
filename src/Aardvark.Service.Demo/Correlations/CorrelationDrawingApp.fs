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
        //let info = System.IO.Directory.CreateDirectory "./saved"
        //let success = info.Exists
        File.WriteAllBytes("./saved/model", arr);
        let success = File.Exists path
        success

    let load (path : CorrelationAppModel) = 
        let arr = File.ReadAllBytes("./saved/model");
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
        | Save
        | Load

                       
    let update (model             : CorrelationAppModel)  
               (semanticApp       : SemanticApp)
               (act : Action)     =
        match act, model.drawing.draw with
            | CameraMessage m, false -> 
                { model with camera = ArcBallController.update model.camera m }      
            | DrawingMessage m, _ ->
                { model with drawing = CorrelationDrawing.update model.drawing semanticApp m }      
            | DrawingSemanticMessage m, _ ->
                {model with drawing = CorrelationDrawing.update model.drawing semanticApp m}          
            | AnnotationMessage m, _ ->
                {model with drawing = CorrelationDrawing.update model.drawing semanticApp m}          
            | KeyDown k, _ -> 
                let d = CorrelationDrawing.update 
                          model.drawing 
                          semanticApp
                          (CorrelationDrawing.Action.KeyDown k)
                { model with drawing = d }
            | KeyUp k, _ -> 
                let d = CorrelationDrawing.update 
                          model.drawing 
                          semanticApp
                          (CorrelationDrawing.Action.KeyUp k)
                { model with drawing = d }
            | Save , _ -> (Serialization.save model "./savedModel") |> ignore
                          model
            | Load , _ -> (Serialization.load model "./savedModel") |> ignore
                          model
            | _ -> model
                       
    let myCss = [
                  { kind = Stylesheet; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
                  { kind = Stylesheet; name = "semui-overrides"; url = "semui-overrides.css" }
                  { kind = Script; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
                ]
    
    let view (model : MCorrelationAppModel) (semanticApp : MSemanticApp)=
        let frustum =
            Mod.constant (Frustum.perspective 60.0 0.1 100.0 1.0)

                 
        require (myCss) (
            body [clazz "ui"; style "background: #1B1C1E; width: 100%; height:100%; overflow: auto;"] [
              div [] [
                ArcBallController.controlledControl model.camera CameraMessage frustum
                    (AttributeMap.ofList [
                                onKeyDown (KeyDown)
                                onKeyUp (KeyUp)
                                attribute "style" "width:70%; height: 100%; float: left;"]
                    )
                    (
                        CorrelationDrawing.Sg.view model.drawing semanticApp model.camera.view
                            |> Sg.map DrawingMessage
                            |> Sg.fillMode (model.rendering.fillMode)
                                    
                            |> Sg.cullMode (model.rendering.cullMode)                                                                    
                    )                                        
              ]
            
              div [clazz "scrolling content"; style "width:30%; height: 100%; float: right; overflow-y: scroll"] [
                  CorrelationDrawing.UI.viewAnnotations model.drawing semanticApp |> UI.map AnnotationMessage   
                  //CorrelationDrawing.SemanticApp.viewSemantics model. |> UI.map DrawingSemanticMessage

            ]
        ]
        ) 

//    let view' (model : MCorrelationAppModel) =
//      view model SemanticApp.initial

    let initial : CorrelationAppModel =
        {
            camera    = { ArcBallController.initial with view = CameraView.lookAt (23.0 * V3d.OIO) V3d.Zero Mars.Terrain.up}
            rendering = RenderingPars.initial
            drawing   = CorrelationDrawing.initial 
        }

//    let app : App<CorrelationAppModel,MCorrelationAppModel,Action> =
//        {
//            unpersist = Unpersist.instance
//            threads = fun model -> ArcBallController.threads model.camera |> ThreadPool.map CameraMessage
//            initial = initial
//            update = update
//            view = view'
//        }
//
//    let start () = App.start app

