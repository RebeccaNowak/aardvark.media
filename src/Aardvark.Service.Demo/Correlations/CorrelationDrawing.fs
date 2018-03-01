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
    open CorrelationUtilities

    let initial : CorrelationDrawingModel = {
        draw = false
        hoverPosition = None
        working = None
        projection = Projection.Viewpoint
        geometry = GeometryType.Line
        semantics = hmap.Empty
        semanticsList = plist.Empty
        selectedSemantic = "" //TODO?
        annotations = plist.Empty
        exportPath = @"."
    }

    type Action =
        | SetSemantic       of string
        | AddSemantic
        | DoNothing
        | SemanticMessage   of Semantic.Action
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

    let insertFirstSemantics (model : CorrelationDrawingModel) = 

        let id = Guid.NewGuid().ToString()

        let newSem = Semantic.initial id
        let newSemantics = HMap.union model.semantics (model.semantics.Add(newSem.id, newSem))
        {model with semantics = newSemantics; semanticsList = plistFromHMap newSemantics; selectedSemantic = newSem.id}
           
    let disableSemantic (semO : Option<Semantic>) =
        match semO with
            | Some s -> Some(Semantic.update s Semantic.Disable)
            | None -> None

    let enableSemantic (semO : Option<Semantic>) =
        match semO with
            | Some s -> Some(Semantic.update s Semantic.Enable)
            | None -> None //TODO something useful


    let insertSampleSemantics (model : CorrelationDrawingModel) = 
        // disabled previous

        let id = Guid.NewGuid().ToString()
        let newSemantic = {(Semantic.initial id) with label = (sprintf "Semantic%i" (model.semantics.Count + 1))}
        let newSemantics = (model.semantics.Add(newSemantic.id, newSemantic)
            |> HMap.alter model.selectedSemantic disableSemantic
            |> HMap.alter newSemantic.id enableSemantic)
                               
        let updatedList = sortedPlistFromHmap newSemantics (fun (x : Semantic) -> x.label)
        {model with selectedSemantic = newSemantic.id; semanticsList = updatedList; semantics = newSemantics}
//        {model with 
//            semantics = update3
//            semanticsList = updatedList;
//            selectedSemantic = newSem.id}

        
    let getMSemantic (model : MCorrelationDrawingModel) =
        adaptive {
            let! selected = model.selectedSemantic            
            return AMap.tryFind selected model.semantics
        }
              

    let finishAndAppend (model : CorrelationDrawingModel) = 
        let anns = match model.working with
                            | Some w -> model.annotations |> PList.append w
                            | None -> model.annotations
        { model with working = None; annotations = anns }

//    let getCurrentSemantic (model : CorrelationDrawingModel) =
//        model.semantics.FindIndex(true, (fun x -> model.selectedSemantic <> Some x.id))


        

    let update (model : CorrelationDrawingModel) (act : Action) =
        match (act, model.draw) with
            | DoNothing, _ -> model
            | KeyDown Keys.LeftCtrl, _ ->                     
                    { model with draw = true }
            | KeyUp Keys.LeftCtrl, _ -> 
                    {model with draw = false; hoverPosition = None }
            | Move p, true -> 
                    { model with hoverPosition = Some (Trafo3d.Translation p) }
            | AddPoint p, true -> 
                    let working = 
                        match model.working with
                                | Some w ->                                     
                                    { w with points = w.points |> PList.append p }
                                | None ->                                     
                                    {Annotation.initial with
                                        points = PList.ofList [p]; 
                                        semanticId = model.selectedSemantic}//add annotation states

                    let model = { model with working = Some working }

                    let model = match (working.geometry, (working.points |> PList.count)) with
                                    | GeometryType.Point, 1 -> model |> finishAndAppend
                                    | GeometryType.Line, 10 -> model |> finishAndAppend
                                    | _ -> model

                    model                 
                    
            | KeyDown Keys.Enter, _ -> 
                    model |> finishAndAppend
            | Exit, _ -> 
                    { model with hoverPosition = None }
            | SetSemantic sem, _ ->
                    let update1 = HMap.alter model.selectedSemantic disableSemantic model.semantics
                    let update2 = HMap.alter sem enableSemantic update1
                    
                    let updatedList = sortedPlistFromHmap update2 (fun (x : Semantic) -> x.label)
                        
//                        model.semanticsList  
//                            |> PList.toList 
//                            |> List.sortBy  (fun (x : Semantic) -> x.label) 
//                            |> PList.ofList
                    // (PList.sortBy (fun (x : Semantic) -> x.label)) model.semanticsList
                    {model with selectedSemantic = sem; semanticsList = updatedList; semantics = update2}
            | SemanticMessage sem, _ ->
                    let fUpdate (semO : Option<Semantic>) = 
                        match semO with
                            | Some s -> Some( Semantic.update s sem)
                            | None -> None //TODO something useful
                    let updatedSemantics = (HMap.alter model.selectedSemantic fUpdate model.semantics)
                    {model with semantics = updatedSemantics; semanticsList = (sortedPlistFromHmap updatedSemantics (fun (x : Semantic) -> x.label))}
            | AddSemantic, _ -> insertSampleSemantics model 
            | SetGeometry mode, _ ->
                    { model with geometry = mode }
            | SetProjection mode, _ ->
                    { model with projection = mode }
//            | KeyDown Keys.D0, _ -> 
//                    {model with semantic = Semantic.Horizon0 }               
//            | KeyDown Keys.D1, _ -> 
//                    {model with semantic = Semantic.Horizon1 }               
//            | KeyDown Keys.D2, _ -> 
//                    {model with semantic = Semantic.Horizon2 }               
//            | KeyDown Keys.D3, _ -> 
//                    {model with semantic = Semantic.Horizon3 }               
//            | KeyDown Keys.D4, _ -> 
//                    {model with semantic = Semantic.Horizon4 }               
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
       
        let viewAnnotationTools (model:MCorrelationDrawingModel) =  
            let selected = getMSemantic model
            let onChange =
                fun (selected : option<MSemantic>) ->
                    match selected with
                        | Some d -> SetSemantic (Mod.force d.id)
                        | None -> DoNothing //AddSemantic // TODO?
                   
                           

            Html.SemUi.accordion "Annotation Tools" "Write" true [
                Html.table [                            
                    Html.row "Text:"        [Html.SemUi.textBox  model.exportPath SetExportPath ]
                    Html.row "Geometry:"    [Html.SemUi.dropDown model.geometry   SetGeometry]
                    Html.row "Projections:" [Html.SemUi.dropDown model.projection SetProjection]
                    Html.row "Semantic:"    
                        [dropDownListR 
                            model.semanticsList 
                            (getMSemantic model) 
                            onChange 
                            (fun x -> x.label) 
                            (fun x -> (Mod.map (fun y -> not y) x.disabled))]
               ]                               
            ]   
            

        let viewAnnotations (model : MCorrelationDrawingModel) = 
          Html.SemUi.accordion "Annotations" "File Outline" true [
              Incremental.div 
                  (AttributeMap.ofList [clazz "ui list"]) (
                      alist {                                                                     
                          for a in model.annotations do    
                            yield Annotation.view model a |> UI.map AnnotationMessage
                      }     
              )
          ]

        let viewSemantics (model : MCorrelationDrawingModel) = 
          Html.SemUi.accordion "Semantics" "File Outline" true [
            Incremental.div
              (AttributeMap.ofList [clazz "ui list"]) (
                alist {
                  for mSem in model.semanticsList do
                    let! domNode = Semantic.view mSem
                    yield div [clazz "item";style "padding: 0px 0px 0px 0px"] [domNode |> UI.map SemanticMessage]
                           // if mSem.id = model.selectedSemantic
                           //     then yield Semantic.viewDisabled mSem |> UI.map SemanticMessage
                           //     else yield Semantic.viewEnabled mSem |> UI.map SemanticMessage
                              //yield Semantic.view mSem |> UI.map SemanticMessage
                }
              )
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

        let mkISg color size trafo =         
            Sg.sphere 5 color size 
                    |> Sg.shader {
                        do! DefaultSurfaces.trafo
                        do! DefaultSurfaces.vertexColor
                        do! DefaultSurfaces.simpleLighting
                    }
                    |> Sg.noEvents
                    |> Sg.trafo(trafo) 
        
        let canvas =             
            Sg.sphere' 8 (new C4b(247,127,90)) 20.0
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.vertexColor
                    do! DefaultSurfaces.simpleLighting
                }
                |> Sg.requirePicking
                |> Sg.noEvents 
                    |> Sg.withEvents [
                        Sg.onMouseMove (fun p -> (Action.Move p))
                        Sg.onClick(fun p -> Action.AddPoint p)
                        Sg.onLeave (fun _ -> Action.Exit)
                    ]  
                |> Sg.onOff (Mod.constant true)
              //  |> Sg.map DrawingMessage

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
            
        let brush (hovered : IMod<Trafo3d option>) = 
            let trafo =
                hovered |> Mod.map (function o -> match o with 
                                                    | Some t-> t
                                                    | None -> Trafo3d.Scale(V3d.Zero))

            mkISg (Mod.constant C4b.Red) (Mod.constant 0.05) trafo
       
        let dots (points : alist<V3d>) (color : IMod<C4b>) (view : IMod<CameraView>) =            
            
            aset {
                for p in points |> ASet.ofAList do
                    yield mkISg color (computeScale view (Mod.constant p) 5.0) (Mod.constant (Trafo3d.Translation(p)))
            } 
            |> Sg.set
           
        let lines (points : alist<V3d>) (color : IMod<C4b>) (width : IMod<float>) = 
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
   
        //let getC4bFromCI = Mod.map (fun c -> c.c)

        let annotation (model : MCorrelationDrawingModel) (anno : IMod<Option<MAnnotation>>)(view : IMod<CameraView>) = 
            //alist builder?
            let points = 
                anno |> AList.bind (fun o -> 
                    match o with
                        | Some a -> a.points
                        | None -> AList.empty
                )    
                
            let withDefault (m : IMod<Option<'a>>) (f : 'a -> IMod<'b>) (defaultValue : 'b) = 
                let defaultValue = defaultValue |> Mod.constant
                m |> Mod.bind (function | None -> defaultValue | Some v -> f v)
            let color = Annotation.getColorMod' anno model
            let thickness = Annotation.getThicknessMod' anno model
            [lines points color thickness; dots points color view]
          
        let annotation' (model : MCorrelationDrawingModel)(anno : MAnnotation)(view : IMod<CameraView>) =      
            let color = Annotation.getColorMod anno model
            let thickness = Annotation.getThicknessMod anno model
            [lines anno.points color thickness; dots anno.points color view] |> ASet.ofList

        let view (model:MCorrelationDrawingModel) (cam:IMod<CameraView>) =        
            // order is irrelevant for rendering. change list to set,
            // since set provides more degrees of freedom for the compiler
            let annoSet = ASet.ofAList model.annotations 

            let annotations =
                aset {
                    for a in annoSet do
                        yield! annotation' model a cam
                } |> Sg.set
                                

            [canvas; brush model.hoverPosition; annotations] @ annotation model model.working cam
            |> Sg.ofList