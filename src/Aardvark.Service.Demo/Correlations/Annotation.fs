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
          points          = plist<AnnotationPoint>.Empty
          segments        = plist.Empty
          projection      = Projection.Viewpoint
          visible         = true
          text            = "text"
          overrideStyle   = None
          overrideLevel   = None
          grainSize       = rand.NextDouble() * 5.0
          selected        = false
          hovered         = false
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
        selected        = false
        hovered         = false
    }

  type Action =
      | SetSemantic     of option<string>
      | ToggleSelected  of V3d
      | Select          of V3d 
      | HoverIn
      | HoverOut
      | Deselect

  let update  (a : Action) (anno : Annotation) =
      match a with
          | SetSemantic str -> match str with
                                | Some s -> {anno with semanticId = s}
                                | None -> anno
          | ToggleSelected (point) -> 
            let ind =       
              anno.points.FirstIndexOf(fun (p : AnnotationPoint) -> V3d.AllEqual(p.point,point)) 
            let upd = anno.points.Update (ind, (fun (p : AnnotationPoint) -> {p with selected = not p.selected}))
            {anno with points = upd}
          | Select (point) ->
            let ind =       
              anno.points.FirstIndexOf(fun (p : AnnotationPoint) -> V3d.AllEqual(p.point,point)) 
            let upd = anno.points.Update (ind, (fun (p : AnnotationPoint) -> {p with selected = true}))
            {anno with points = upd}
          | HoverIn  -> 
            {anno with hovered        = true
                       overrideStyle  = Some {Style.color      = {c = C4b.Yellow};
                                              Style.thickness  = {Numeric.init with value = 3.0}
                                             }
            }
          | HoverOut -> {anno with hovered = false
                                   overrideStyle = None}
          | Deselect -> {anno with points = anno.points |> PList.map (fun p -> {p with selected = false})}

  let viewSelected (model : MAnnotation)  (semanticApp : MSemanticApp) = 
    let semanticsNode = 
      let iconAttr =
        amap {
          yield clazz "circle outline icon"
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

  let viewDeselected (model : MAnnotation)  (semanticApp : MSemanticApp) = 
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


  let view  (model : MAnnotation)  (semanticApp : MSemanticApp) = 
    model.selected
      |> Mod.map (fun d -> 
          match d with
            | true  -> viewSelected model semanticApp
            | false -> viewDeselected model semanticApp)
 
    
        
  ///////// HELPER FUNCTIONS
  let getColor (imAnno : IMod<Option<MAnnotation>>) (semanticApp : MSemanticApp) =
    adaptive {
      let! optAnno = imAnno
      let (a : IMod<C4b>, b : IMod<bool>) = 
          match optAnno with
            | Some an -> 
              let col =
                an.overrideStyle 
                  |> Mod.bind (fun (st : Option<MStyle>) ->
                                match st with
                                  | Some st -> st.color.c
                                  | None -> ((SemanticApp.getColor semanticApp an.semanticId))
                              )
              (col, an.hovered)
//                let! style = a.overrideStyle
//                match style with
//                  | Some st -> st.color
//                  | None -> ((SemanticApp.getColor semanticApp a.semanticId), a.hovered)
//              foo
            //let foo = ((SemanticApp.getColor semanticApp a.semanticId), a.hovered)
            //foo
            | None -> ((Mod.constant C4b.Red), (Mod.constant false))

      let! hover = b
      let! col = a
//      return match hover with
//              | true -> C4b.Yellow
//              | false -> col
      return col
    }
//      Mod.map2 (fun c h -> 
//                  match h with 
//                    | true -> Mod.constant C4b.Yellow
//                    | false -> c
//               ) c h 
  let getColor' (anno : MAnnotation) (semanticApp : MSemanticApp) = 
    adaptive {
      return! anno.overrideStyle 
                |> Mod.bind (fun (st : Option<MStyle>) ->
                    match st with
                      | Some st -> st.color.c
                      | None -> ((SemanticApp.getColor semanticApp anno.semanticId)))
    }
      //let! h = anno.hovered
//      return! match h with
//                | true -> Mod.constant C4b.Yellow
//                | false -> SemanticApp.getColor semanticApp anno.semanticId



    
 

  let getThickness (anno : IMod<Option<MAnnotation>>) (semanticApp : MSemanticApp) = 
    Mod.bind (fun (a : Option<MAnnotation>)
                  -> match a with
                      | Some a -> SemanticApp.getThickness semanticApp a.semanticId
                      | None -> Mod.constant Semantic.ThicknessDefault) anno   

  let getThickness' (anno : MAnnotation) (semanticApp : MSemanticApp) = 
    SemanticApp.getThickness semanticApp anno.semanticId

//  let calcElevation (v : V3d) =
//    v.Y

//  let getAvgElevation (anno : MAnnotation) =
//    anno.points
//      |> AList.averageOf (calcElevation 
//
  let elevation (anno : Annotation) =
    anno.points 
      |> PList.toList
      |> List.map (fun x -> x.point.Length)
      |> List.average
//
//  let getMinElevation (anno : MAnnotation) = 
//    anno.points
//      |> AList.minBy calcElevation
//
//  let getMaxElevation (anno : MAnnotation) = 
//    anno.points
//      |> AList.maxBy calcElevation
//
//  let getRangeMinMax (anno : MAnnotation) =
//    let min = anno.points
//                |> AList.minBy calcElevation
//    let max = anno.points
//                |> AList.maxBy calcElevation
//    Mod.map2 (fun (x : V3d) (y : V3d) -> V2d(x.Y,y.Y)) min max
//
//  let getRange (anno : MAnnotation) =
//    let min = anno.points
//                |> AList.minBy calcElevation
//    let max = anno.points
//                |> AList.maxBy calcElevation
//    Mod.map2 (fun (mi : V3d) (ma : V3d) -> (ma.Y - mi.Y)) min max

//
//  let getMinElevation' (annos : alist<MAnnotation>) =
//    AList.toArray (annos |> AList.map (fun x -> getAvgElevation x))
//      |> Array.min
//
//  let getMaxElevation' (annos : alist<MAnnotation>) =
//    AList.toArray (annos |> AList.map (fun x -> getAvgElevation x))
//      |> Array.max
//
//  let getMinMaxElevation (annos : alist<MAnnotation>) =
//    let avgs = (annos |> AList.map (fun x -> getAvgElevation x))
//    Mod.map2 (fun  (x : float) (y : float) -> V2d(x,y)) (avgs |> AList.min) (avgs |> AList.max)

  let getType (semanticApp : SemanticApp) (anno : Annotation) =
    let s = (SemanticApp.getSemantic semanticApp anno.semanticId)
    match s with
      | Some s -> s.semanticType
      | None   -> SemanticType.Undefined

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

  
      
        
        
      












