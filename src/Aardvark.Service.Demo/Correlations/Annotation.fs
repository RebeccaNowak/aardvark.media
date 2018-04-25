namespace CorrelationDrawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Base.Rendering
open Aardvark.Rendering.Text
open Aardvark.UI
open UtilitiesGUI
open UtilitiesDatastructures


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Annotation =
  let rand = System.Random()
  let initial (id : string) = 
      {     
          id              = id
          semanticType    = SemanticType.Hierarchical
          geometry        = GeometryType.Line
          semanticId      = ""
          points          = plist.Empty
          segments        = plist.Empty
          projection      = Projection.Viewpoint
          visible         = true
          text            = "text"
          overrideStyle   = None
          overrideLevel   = None
          grainSize       = rand.NextDouble() * 5.0
      }

  let initialDummy = 
    {     
        id              = "-1"
        semanticType    = SemanticType.Hierarchical
        geometry        = GeometryType.Line
        semanticId      = ""
        points          = plist.Empty
        segments        = plist.Empty
        projection      = Projection.Viewpoint
        visible         = true
        text            = "dummy"
        overrideStyle   = None
        overrideLevel   = None
        grainSize       = rand.NextDouble() * 5.0
    }

  type Action =
      | SetSemantic of option<string>

  let update (anno : Annotation) (a : Action) =
      match a with
          | SetSemantic str -> match str with
                                | Some s -> {anno with semanticId = s}
                                | None -> anno

  let view (model : MAnnotation)  (semanticApp : MSemanticApp) = 
    let semanticsNode = 
      let iconAttr =
        amap {
          yield clazz "circle icon"
          let! c = SemanticApp.getColor semanticApp model.semanticId
          yield style (sprintf "color:%s" (colorToHexStr c))
//          yield attribute "color" "blue"
        }      
      td [clazz "center aligned"; style lrPadding] 
         [
          Incremental.i (AttributeMap.ofAMap iconAttr) (AList.ofList []);
          //label  [clazz "ui label"] 
                 Incremental.text (SemanticApp.getLabel semanticApp model.semanticId)]

        
    let geometryTypeNode =
      td [clazz "center aligned"; style lrPadding] 
         //[label  [clazz "ui label"] 
                 [text (model.geometry.ToString())]

    let projectionNode =
      td [clazz "center aligned"; style lrPadding] 
         //[label  [clazz "ui label"] 
                 [text (model.projection.ToString())]

    let annotationTextNode = 
        td [clazz "center aligned"; style lrPadding] 
           //[label  [clazz "ui label"] 
                   [Incremental.text model.text]
    
    [semanticsNode;geometryTypeNode;projectionNode;annotationTextNode]
    
        
  ///////// HELPER FUNCTIONS
  let getColor (anno : IMod<Option<MAnnotation>>) (semanticApp : MSemanticApp) = 
    anno |> 
      Mod.bind (fun (a : Option<MAnnotation>) -> 
        match a with
          | Some a -> SemanticApp.getColor semanticApp a.semanticId
          | None -> Mod.constant C4b.Red)

  let getColor' (anno : MAnnotation) (semanticApp : MSemanticApp) = 
    SemanticApp.getColor semanticApp anno.semanticId
    
 
    

  let getThickness (anno : IMod<Option<MAnnotation>>) (semanticApp : MSemanticApp) = 
    Mod.bind (fun (a : Option<MAnnotation>)
                  -> match a with
                      | Some a -> SemanticApp.getThickness semanticApp a.semanticId
                      | None -> Mod.constant Semantic.ThicknessDefault) anno   

  let calcElevation (v : V3d) =
    v.Y

  let getAvgElevation (anno : MAnnotation) =
    anno.points
      |> AList.averageOf calcElevation

  let elevation (anno : Annotation) =
    anno.points.Mean (fun x -> x.Y)

  let getMinElevation (anno : MAnnotation) = 
    anno.points
      |> AList.minBy calcElevation

  let getMaxElevation (anno : MAnnotation) = 
    anno.points
      |> AList.maxBy calcElevation

  let getRangeMinMax (anno : MAnnotation) =
    let min = anno.points
                |> AList.minBy calcElevation
    let max = anno.points
                |> AList.maxBy calcElevation
    Mod.map2 (fun (x : V3d) (y : V3d) -> V2d(x.Y,y.Y)) min max

  let getRange (anno : MAnnotation) =
    let min = anno.points
                |> AList.minBy calcElevation
    let max = anno.points
                |> AList.maxBy calcElevation
    Mod.map2 (fun (mi : V3d) (ma : V3d) -> (ma.Y - mi.Y)) min max


  let getMinElevation' (annos : alist<MAnnotation>) =
    AList.toArray (annos |> AList.map (fun x -> getAvgElevation x))
      |> Array.min

  let getMaxElevation' (annos : alist<MAnnotation>) =
    AList.toArray (annos |> AList.map (fun x -> getAvgElevation x))
      |> Array.max

  let getMinMaxElevation (annos : alist<MAnnotation>) =
    let avgs = (annos |> AList.map (fun x -> getAvgElevation x))
    Mod.map2 (fun  (x : float) (y : float) -> V2d(x,y)) (avgs |> AList.min) (avgs |> AList.max)

  let getLevel (semanticApp : SemanticApp) (anno : Annotation) =
    let s = (SemanticApp.getSemantic semanticApp anno.semanticId)
    match s with
      | Some s -> s.level
      | None   -> -1

  let onlyHierarchicalAnnotations (semanticApp : SemanticApp) (nodes : List<Annotation>) =
    nodes
      |> List.filter (fun (a : Annotation) -> 
      match (SemanticApp.getSemantic semanticApp a.semanticId) with
        | Some s  -> s.semanticType = SemanticType.Hierarchical
        | None    -> false)

  let splitByLevel (semanticApp : SemanticApp) (annos : List<Annotation>) =
    let sem (a : Annotation) =  
      match (SemanticApp.getSemantic semanticApp a.semanticId) with
        | Some s -> s
        | None -> Semantic.initial "-1"

    let levels = 
      annos 
        |> List.map (fun x -> (sem x).level) 
        |> List.distinct
      
    levels 
      |> List.map (fun (lvl : int) ->
                    annos 
                      |> List.filter (fun a -> (sem a).level = lvl))

  let onlyLvli (semanticApp : SemanticApp) (i : int) (annos : List<Annotation>) =
    annos
      |> List.filter (fun (a : Annotation) -> 
      match (SemanticApp.getSemantic semanticApp a.semanticId) with
        | Some s  -> s.level = i
        | None    -> false)

  
      
        
        
      












