namespace CorrelationDrawing
//
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GeologicalLog =

  open System
  open Aardvark.Base
  open Aardvark.Base.Rendering
  open Aardvark.Rendering.Text
  open Aardvark.Base.Incremental
  open Aardvark.Base.Incremental.Operators
  open Aardvark.SceneGraph.SgPrimitives
  open Aardvark.SceneGraph.FShadeSceneGraph
  open Aardvark.UI
  open Aardvark.UI.Primitives
//  open Aardvark.UI.Extensions.Sg
  open Aardvark.SceneGraph
  open UtilitiesRendering
  open Aardvark.Application
  open OrientationCube

  let getSecond (_, s) = s

  let generateNodes (lst : List<V3d * Annotation>) (annos : plist<Annotation>)=
    
    let pwList = 
      lst 
        |> List.pairwise
    seq {
      for ((up, ua), (lp, la)) in pwList do
        let children = 
          //let dir = (lp - up).Normalized
          //let between (a : Annotation) = ((Annotation.elevation a dir) < up.Y) && ((Annotation.elevation a dir) > lp.Y)
          let upper = if up.Y > lp.Y then up.Y else lp.Y
          let lower = if up.Y < lp.Y then up.Y else lp.Y
          let between (a : Annotation) = (upper > (Annotation.elevation a) && (lower < (Annotation.elevation a)))
          annos
            |> PList.filter between

        yield LogNode.initial (up, ua) (lp, la) children
    }
    |> PList.ofSeq
//      |> PList.map
//        (fun (upper, lower) -> LogNode.initial upper lower)
//      |> PList.ofList

  

  let intial (id : string) (lst : List<V3d * Annotation>) (annos : plist<Annotation>) : GeologicalLog = {
    id          = id
    nodes       = generateNodes lst annos
    annoPoints  = lst
    range       = Rangef.init
    camera      = 
      { ArcBallController.initial with 
                     view = CameraView.lookAt (2.0 * V3d.III) V3d.Zero V3d.OOI}    
  }

  type Action =
      | CameraMessage             of ArcBallController.Message    
//      | AddNode                   of (V3d * Annotation)
      

  let update (model : GeologicalLog) (semanticApp : SemanticApp) (msg : Action) =
    match msg with 
      | CameraMessage m -> 
          {model with camera = ArcBallController.update model.camera m}


  let view (model : MGeologicalLog) (semanticApp : MSemanticApp) =
    model.nodes
      |> AList.map (fun n -> LogNode.view n)
      


//  /// HELPER FUNCTIONS
//
//  let addNode (model : GeologicalLog) (anno : Annotation) (semanticApp : SemanticApp) =
//    let glvl = Annotation.getLevel semanticApp
//
//    let newAnnoLst = model.annotations.Append(anno) |> PList.toList
//
//    // filter Annotations: only hierarchical
//    let hier = Annotation.onlyHierarchicalAnnotations semanticApp newAnnoLst
//    
////    let foo =
////      hier
////        |> Annotation.splitByLevel semanticApp
//        //|> List.map (fun lst -> List.sortBy (fun (a : Annotation) -> Annotation.elevation a) lst)
//    
////    let annosToNodes (lst : List<Annotation>) =
////       seq {
////          yield LogNode.initial Annotation.initialDummy lst.Head
////
////          for i in 1 .. (hier.Length - 1) do
////            yield LogNode.initial (hier.Item (i-1)) (hier.Item (i))
////        }
//
//    
////    let newNodes = 
////      hier
////        |> Annotation.onlyLvli semanticApp 0
////        |> annosToNodes
//    
////    let split =
////      hier
////        |> Annotation.splitAtLvli semanticApp 0
//
//    
//
////    let foo =
////      hier
////        |> List.sortBy (fun x -> Annotation.elevation x)
//
////    let bar =
////      seq {
////        yield LogNode.initial Annotation.initialDummy hier.Head
////
////        for i in 1 .. (hier.Length - 1) do
////          if (glvl (hier.Item (i-1))) = (glvl (hier.Item (i))) then
////            yield (LogNode.initial (hier.Item (i-1)) (hier.Item (i)))
////          else if (glvl (hier.Item (i-1))) < (glvl (hier.Item (i))) then
////            // new Layer
////            yield (LogNode.initial (hier.Item (i-1)) (hier.Item (i)))
////
////      }
//
//          
//      
//
//
//
////    let newNode = LogNode.initial anno anno /////// 
////    let onlyHNodes = 
////      (model.nodes.Append newNode)
////        |> PList.toList
////        |> (LogNode.onlyHierarchicalNodes semanticApp)
//
//            
//
////    let newNodes = 
////      onlyHNodes
////          |> List.sortBy (fun (x : LogNode) -> x.elevation)
////          |> PList.ofList
////          |> PList.mapiInt
////          |> PList.map (fun (x, i) -> {x with index = i}) 
////          |> PList.map LogNode.recalcRangeAndSize
//        
//    let nodesWithPositions = 
////      let lst = 
////        onlyHNodes
////          //|> PList.toList
////          |> List.sortBy (fun x -> x.elevation)
//      newNodes  
//        |> Seq.toList
//        |> (LogNode.calcPos' 10.0)
//        |> PList.ofList
//    
//    {model with nodes = nodesWithPositions}  
//
//    
//
  
//
//
//
//
//
//  let inline (==>) a b = Attributes.attribute a b
//
//  let createAnnoBox (node : MLogNode) (colour : IMod<C4b>) =
//    let foo = C4b(0.0, 0.0, 0.0, 0.5)
//    let box =
//        let c = colour |> Mod.map ( fun col -> C4b(col.R, col.G, col.B, foo.A)) 
//        let b = Mod.map2 (fun pos size -> (Box3d.FromCenterAndSize(pos, size))) node.pos node.size
//        let lbl = Mod.map2 (fun (x : V3d) (y : float) -> sprintf "%f / %f" x.Y y) node.size node.logYPos
//        let lblSg = (UtilitiesRendering.makeLblSg' lbl node.pos)
//
//        (Sg.box c b)                   
//          |> Sg.shader {
//              do! DefaultSurfaces.trafo
//              do! DefaultSurfaces.vertexColor
//              do! DefaultSurfaces.simpleLighting
//          } 
//
//      |> Sg.andAlso lblSg  
//          //|> Sg.noEvents
//          //|> Sg.translate' (Mod.constant (V3d.OIO * pos))
//          //|> Sg.billboard
//      
//
//    box
//
//
//
//  let sg (model       : MGeologicalLog)
//         (annos       : alist<MAnnotation>) 
//         (semanticApp : MSemanticApp) 
//         (camView     : IMod<CameraView>) = //(pos : IMod<V3d>) =
//    if (Mod.force annos.Content).Count < 2 then
//      Sg.empty
//    else  
//      alist {
//        for node in model.nodes do
//          yield createAnnoBox node (Annotation.getColor' node.lBoundary semanticApp) 
//      }
//      |> AList.toASet
//      |> Sg.set
//
//
//    
//
//
//
//
//
//
//
//  let view (runtime : IRuntime) (model : MGeologicalLog) =
//
//    let domNode =
//      let attributes =
//        AttributeMap.ofList [
//              attribute "style" "width:100%; height: 100%;border: 1px solid black"
//              clazz "svgRoot"; 
//        ]
//
//      ArcBallController.controlledControl 
//        model.camera
//        CameraMessage 
//        (Mod.constant (Frustum.perspective 60.0 0.1 100.0 1.0))
//        (AttributeMap.ofList [
//          attribute "style" "width:100%; height: 100%;border: 1px solid black"
//          clazz "svgRoot"; 
//        ])
//        (
//          //sg model.camera.view (Mod.constant V3d.OOO)
//          renderLblTextureQuad runtime model.camera.view
//        )
//
//    body [clazz "ui"; style "background: #1B1C1E; width: 100%; height:100%; overflow: auto;"] [
//      div [] [
//        domNode
//      ]
//    ]
//    
//    ////////////
//
//    //            let heightList = 
////              annos 
////                |> AList.map (fun x -> Annotation.getRange x)
////                |> AList.toList
////                
////
////            let annoKeys =
////              annos 
////                |> AList.map (fun x -> x.id)
////                |> AList.toList
////            
////            let f (i : int) (value : IMod<float>) (valLst : List<IMod<float>>) =
////              value 
////                |> Mod.map (fun (x : float) ->
////                              match x with
////                                | h when i < 1 -> x
////                                | h when i >= 1 -> x + (Mod.force (valLst.Item (i - 1)))
////                                | _ -> 0.0)
////
////            let accHeightList = (heightList 
////                                  |> List.mapi (fun i x -> f i x heightList))
////
////
////            let heightMap = 
////              List.zip annoKeys accHeightList
////              |> AMap.ofList
////
////            //let minMax = Annotation.getMinMaxElevation annos
////
////
////            let annoSet = ASet.ofAList annos
////         
//////
////            let sgSet = 
////              aset {
////                for anno in annoSet do
////                  let pos = AMap.find anno.id heightMap
////                  let tmp = (Annotation.getRange anno) |> Mod.map 
////                              (fun el -> (createAnnoBox anno semanticApp camView (pos |> Mod.force |> Mod.force)))
////                  yield tmp |> Sg.dynamic
////              }
//
////            sgSet |> Sg.set
//
////    (Mod.map sgSetLayers (AList.isEmpty annos))
////      |> Sg.dynamic
//
//
//
/////////////////////////
//
//
////  let createAnnoBox (anno : MAnnotation) 
////                    (semanticApp : MSemanticApp) 
////                    (view : IMod<CameraView>) 
////                    (pos : float) = 
//////    let range =
//////      (Annotation.getRange anno) 
//////    let boxPos = V3d.IOI + (V3d.OIO * pos)
//////    Sg.box
//////      (Annotation.getColor' anno semanticApp)  
//////      (range
//////        |> (Mod.map 
//////              (fun x -> 
//////                (Box3d.FromCenterAndSize(boxPos, V3d.OIO * 2.0))
//////              )
//////            )
//////      )
////    Sg.box (Annotation.getColor' anno semanticApp) 
////           (Mod.constant (Box3d.FromCenterAndSize(V3d.III, V3d.III * 5.0)))
////      |> Sg.shader {
////          do! DefaultSurfaces.trafo
////          do! DefaultSurfaces.vertexColor
////          do! DefaultSurfaces.simpleLighting
////      }         
////          //|> Sg.trafo scale
////      //|> Sg.trafo ( view |> Mod.map ( fun v -> Trafo3d.RotateInto(V3d.OOI, v.Sky) ) )
////      |> Sg.noEvents
////      //|> Sg.translate' (Mod.constant (V3d.OIO * pos))
////      |> Sg.billboard
