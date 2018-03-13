namespace CorrelationDrawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Base.Rendering
open Aardvark.UI
open UtilitiesGUI


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Annotation =

  let initial (id : string) (semanticsList : plist<Semantic>) = 
      {     
          id = id

          geometry = GeometryType.Line
          semanticId = ""
          points = plist.Empty
          segments = plist.Empty
          projection = Projection.Viewpoint
          visible = true
          text = "text"
      }

  type Action =
      | SetSemantic of option<string>

  let update (anno : Annotation) (a : Action) =
      match a with
          | SetSemantic str -> match str with
                                | Some s -> {anno with semanticId = s}
                                | None -> anno

  let view (model : MAnnotation) (cdModel : MCorrelationDrawingModel) = 
    let semanticsNode =
      td [clazz "center aligned"; style lrPadding] 
         [(DropdownList.view' cdModel.semanticsList
                              (Option.map (fun y -> y.id) >> SetSemantic)
                              (fun x -> x.label.text)
                              (fun x -> Mod.map (fun y -> x.id = y) model.semanticId))]
          
    let geometryTypeNode =
      td [clazz "center aligned"; style lrPadding] 
         [label  [clazz "ui label"] 
                 [text (model.geometry.ToString())]]

    let projectionNode =
      td [clazz "center aligned"; style lrPadding] 
         [label  [clazz "ui label"] 
                 [text (model.projection.ToString())]]

    let annotationTextNode = 
        td [clazz "center aligned"; style lrPadding] 
           [label  [clazz "ui label"] 
                   [Incremental.text model.text]]
    
    [semanticsNode;geometryTypeNode;projectionNode;annotationTextNode]
    
        
  /////////////////////////

        //@Thomas: is there a simpler way to do this?
    //@Thomas: performance: use of Mod.bind
  let getColor (anno : IMod<Option<MAnnotation>>) (cdModel : MCorrelationDrawingModel) = 
    Mod.bind (fun (a : Option<MAnnotation>)
                  -> match a with
                      | Some a ->
                          let sem = Mod.bind (fun id -> AMap.tryFind id cdModel.semantics) a.semanticId
                          Mod.bind (fun (se : option<MSemantic>) ->
                            match se with
                                        | Some s -> s.style.color.c
                                        | None -> Mod.constant C4b.Red) sem
                      | None -> Mod.constant C4b.Red) anno   


  let getColor' (anno : MAnnotation) (cdModel : MCorrelationDrawingModel) = 
    let sem = Mod.bind (fun id -> AMap.tryFind id cdModel.semantics) anno.semanticId
    Mod.bind (fun (se : option<MSemantic>) ->
      match se with
                  | Some s -> s.style.color.c
                  | None -> Mod.constant C4b.Red) sem

  
    

  let getThickness (anno : IMod<Option<MAnnotation>>) (cdModel : MCorrelationDrawingModel) = 
    Mod.bind (fun (a : Option<MAnnotation>)
                  -> match a with
                      | Some a ->
                          let sem = Mod.bind (fun id -> AMap.tryFind id cdModel.semantics) a.semanticId
                          Mod.bind (fun (se : option<MSemantic>) ->
                            match se with
                                        | Some s -> s.style.thickness.value
                                        | None -> Mod.constant Semantic.ThicknessDefault) sem
                      | None -> Mod.constant Semantic.ThicknessDefault) anno   


  let getThickness' (anno : MAnnotation) (cdModel : MCorrelationDrawingModel) = 
    let sem = Mod.bind (fun id -> AMap.tryFind id cdModel.semantics) anno.semanticId
    Mod.bind (fun (se : option<MSemantic>) ->
      match se with
                  | Some s -> s.style.thickness.value
                  | None -> Mod.constant Semantic.ThicknessDefault) sem      
                                               
        

