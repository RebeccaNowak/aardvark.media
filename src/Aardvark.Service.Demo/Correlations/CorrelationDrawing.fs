namespace CorrelationDrawing





module CorrelationDrawing =
    open Newtonsoft.Json
   // open PRo3DModels
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI

    open System
    
    open Aardvark.Base.Geometry
    open Aardvark.Base.Incremental
    open Aardvark.Base.Incremental.Operators
    open Aardvark.Base.Rendering
    open Aardvark.Application
    open Aardvark.SceneGraph
    open Aardvark.UI
    open Aardvark.UI.Primitives
    open Aardvark.Rendering.Text

    open Aardvark.SceneGraph.SgPrimitives
    open Aardvark.SceneGraph.FShadeSceneGraph
    open Annotation
    open UtilitiesGUI
    open UtilitiesDatastructures

    let initial : CorrelationDrawingModel = {
        draw = false
        hoverPosition = None
        working = None
        projection = Projection.Viewpoint
        geometry = GeometryType.Line
        selectedAnnotation = None
        annotations = plist.Empty
        exportPath = @"."
        log = GeologicalLog.intial (Guid.NewGuid().ToString())
    }

    type Action =
        | SetSemantic       of option<string>
        | AddSemantic
        | DoNothing
        | AnnotationMessage of Annotation.Action
        | SetGeometry       of GeometryType
        | SetProjection     of Projection
        | SetExportPath     of string
        | Move              of V3d
        | Exit    
        | AddPoint          of V3d
        | KeyDown           of key : Keys
        | KeyUp             of key : Keys      
        | Export
           
       
    let finishAndAppend (model : CorrelationDrawingModel) = 
        let (anns, log) = match model.working with
                            | Some w ->
                                let newLog = GeologicalLog.update model.log (GeologicalLog.AddNode w)
                                let annos  = model.annotations |> PList.append w
                                (annos, newLog)
                            | None -> (model.annotations, model.log)

        { model with  working       = None 
                      annotations   = anns
                      log           = log}


    let update (model : CorrelationDrawingModel) (selectedSemantic : string) (act : Action) =
        match (act, model.draw) with
            | DoNothing, _ -> model
            | KeyDown Keys.LeftCtrl, _ ->                     
                { model with draw = true }
            | KeyUp Keys.LeftCtrl, _ -> 
                {model with draw = false; hoverPosition = None }
            | Move p, true -> 
                { model with hoverPosition = Some (Trafo3d.Translation p) }
            | AddPoint m, true -> 
                let working = 
                  match model.working with
                    | Some w  ->                                     
                        let newAnno = { w with points = w.points |> PList.append m }
                        newAnno
                    | None    -> 
                        let id      = Guid.NewGuid().ToString()                                     
                        let newAnno = {Annotation.initial id with
                                        points      = PList.ofList [m];  
                                        semanticId  = selectedSemantic
                                        geometry    = model.geometry
                                        projection  = model.projection}//add annotation states
                        newAnno

                let model = {model with working  = Some working}

                let model = match (working.geometry, (working.points |> PList.count)) with
                              | GeometryType.Point, 1 -> model |> finishAndAppend
                              | GeometryType.Line, 10 -> model |> finishAndAppend
                              | _                     -> model

                model                 
                
            | KeyDown Keys.Enter, _ -> 
                    model |> finishAndAppend
            | Exit, _ -> 
                    { model with hoverPosition = None }
            | SetGeometry mode, _ ->
                    { model with geometry = mode }
            | SetProjection mode, _ ->
                    { model with projection = mode }        
            | SetExportPath s, _ ->
                    { model with exportPath = s }
            | Export, _ ->
                    //let path = Path.combine([model.exportPath; "drawing.json"])
                    //printf "Writing %i annotations to %s" (model.annotations |> PList.count) path
                    //let json = model.annotations |> PList.map JsonTypes.ofAnnotation |> JsonConvert.SerializeObject
                    //Serialization.writeToFile path json 
                    
                    model
            | _ -> model

    module UI =
        open Aardvark.Base.Incremental    
       
        let viewAnnotationTools (model:MCorrelationDrawingModel) (semanticApp : MSemanticApp)=  
          //let selected = getMSemantic model
          let onChange =
              fun (selected : option<MSemantic>) ->
                  match selected with
                      | Some d -> SetSemantic (Some d.id)
                      | None -> DoNothing //AddSemantic // TODO?
                   
          let mapping = Option.map (fun (y : MSemantic) -> y.id)
       
          [div [clazz "item"] 
              [div [clazz "ui small right labeled input"] [
                      label [clazz "ui basic label"] [text "Geometry"]  // style "color:white"
                      Html.SemUi.dropDown model.geometry SetGeometry]];
          div [clazz "item"] 
              [div [clazz "ui small right labeled input"] [
                      label [clazz "ui basic label"] [text "Projections"]
                      Html.SemUi.dropDown model.projection SetProjection]]]



        let viewAnnotations (model : MCorrelationDrawingModel) (semanticApp : MSemanticApp) = 
          let domList = 
            alist {
              for a in model.annotations do
                yield (tr 
                  ([style tinyPadding])
                  (List.map (fun x -> x |> UI.map AnnotationMessage) (Annotation.view a semanticApp))
                )
            }  
          div [] [
            div[clazz "ui compact horizontal inverted menu"; 
                style "float:middle; vertical-align: middle"] 
                (viewAnnotationTools model semanticApp)
            Html.SemUi.accordion "Annotations" "File Outline" true [
              table 
                ([clazz "ui celled striped inverted table unstackable"; 
                  style "padding: 1px 5px 1px 5px"]) 
                [thead [][tr[][th[][text "Semantic"];
                                      th[][text "Geometry"];
                                      th[][text "Projection"];
                                      th[][text "Text"]
                          ]];
                Incremental.tbody (AttributeMap.ofList []) domList]    
            ]
          ]
          


    module Sg =        
        let computeScale (view : IMod<CameraView>)(p:IMod<V3d>)(size:float) =
            adaptive {
                let! p = p
                let! v = view
                let distV = p - v.Location
                let distF = V3d.Dot(v.Forward, distV)
                return distF * size / 800.0 //needs hfov at this point
            }
        
        let makeLblSg (str : string) (pos : V3d) =
          Sg.text (Font.create "courier" FontStyle.Regular) C4b.White (Mod.constant str)
              |> Sg.billboard
              |> Sg.noEvents
              |> Sg.depthTest (Mod.constant DepthTestMode.None)
              |> Sg.trafo(Mod.constant (Trafo3d.Translation pos))          


        let pick = 
          Mars.Terrain.pickSg [
            Sg.onMouseMove (fun p -> (Action.Move p))
            Sg.onClick(fun p -> Action.AddPoint p)
            Sg.onLeave (fun _ -> Action.Exit)
            ]

        let edgeLines (close : bool) (points : alist<V3d>) =
            points |> AList.toMod |> Mod.map (fun l ->
                let list = PList.toList l
                let head = list |> List.tryHead
                    
                match head with
                    | Some h -> if close then list @ [h] else list
                                    |> List.pairwise
                                    |> List.map (fun (a,b) -> new Line3d(a,b))
                                    |> List.toArray
                    | None -> [||]                         
            )

        let makeSphereSg color size trafo =      
          Sg.sphere 5 color size 
                  |> Sg.shader {
                      do! DefaultSurfaces.trafo
                      do! DefaultSurfaces.vertexColor
                      do! DefaultSurfaces.simpleLighting
                  }
                  |> Sg.noEvents
                  |> Sg.trafo(trafo)
            
        let makeBrushSg (hovered : IMod<Trafo3d option>) (color : IMod<C4b>) = //(size : IMod<float>)= 
            let trafo =
                hovered |> Mod.map (function o -> match o with 
                                                    | Some t-> t
                                                    | None -> Trafo3d.Scale(V3d.Zero))
            makeSphereSg (color) (Mod.constant 0.05) trafo
       
        let makeDotsSg (points : alist<V3d>) (color : IMod<C4b>) (view : IMod<CameraView>) =            
            
            aset {
                for p in points |> ASet.ofAList do
                    yield makeSphereSg 
                            color 
                            (computeScale view (Mod.constant p) 5.0) 
                            (Mod.constant (Trafo3d.Translation(p)))
            } 
            |> Sg.set
           
        let makeLinesSg (points : alist<V3d>) (color : IMod<C4b>) (width : IMod<float>) = 
            edgeLines false points
                |> Sg.lines color
                |> Sg.effect [
                    toEffect DefaultSurfaces.trafo
                    toEffect DefaultSurfaces.vertexColor
                    toEffect DefaultSurfaces.thickLine                                
                    ] 
                |> Sg.noEvents
                |> Sg.uniform "LineWidth" width
                |> Sg.pass (RenderPass.after "lines" RenderPassOrder.Arbitrary RenderPass.main)
                |> Sg.depthTest (Mod.constant DepthTestMode.None)

        let createAnnotationSgs (semanticApp : MSemanticApp)
                                (anno        : IMod<Option<MAnnotation>>)
                                (view        : IMod<CameraView>) = 
          let points = 
              anno |> AList.bind (fun o -> 
                  match o with
                      | Some a -> a.points
                      | None -> AList.empty
              )    
                
          let withDefault (m : IMod<Option<'a>>) (f : 'a -> IMod<'b>) (defaultValue : 'b) = 
              let defaultValue = defaultValue |> Mod.constant
              m |> Mod.bind (function | None -> defaultValue | Some v -> f v)
          let color = Annotation.getColor anno semanticApp
          let thickness = Annotation.getThickness anno semanticApp 
          [makeLinesSg points color thickness; makeDotsSg points color view]
          

        let createAnnotationSgs' (semanticApp  : MSemanticApp)
                        (anno         : MAnnotation)
                        (view         : IMod<CameraView>) =      
          let color = SemanticApp.getColor semanticApp anno.semanticId
          let thickness = SemanticApp.getThickness semanticApp anno.semanticId
          [makeLinesSg anno.points color thickness;
           makeDotsSg anno.points color view;
          ] |> ASet.ofList




        let view (model       : MCorrelationDrawingModel)
                 (semanticApp : MSemanticApp) 
                 (cam         : IMod<CameraView>) =        
            // order is irrelevant for rendering. change list to set,
            // since set provides more degrees of freedom for the compiler
            let annoSet = ASet.ofAList model.annotations 

            let annotations =
                let id = Guid.NewGuid().ToString()   
               
                aset {
                    for a in annoSet do
                        yield! createAnnotationSgs' semanticApp a cam
                } |> Sg.set         
                

            [
              [Mars.Terrain.mkISg() 
                |> Sg.effect Mars.Terrain.defaultEffects
                |> Sg.noEvents
              ] 
              |> Sg.ofList;
              pick;
              makeBrushSg model.hoverPosition (SemanticApp.getColor semanticApp semanticApp.selectedSemantic);
              annotations;
              (GeologicalLog.sg model.log model.annotations semanticApp cam) |> Sg.noEvents
            ] @ createAnnotationSgs semanticApp model.working cam
            |> Sg.ofList
            
            
