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

  // let annotationToNode (anno : Annotation) (semApp : SemanticApp) =

  let generateNonLevelNodes (annos : plist<Annotation>) (semApp : SemanticApp) =
    
    annos 
      |> PList.map 
        (fun a -> 
          match (Annotation.getType semApp a) with
           | SemanticType.Hierarchical -> (LogNode.initialEmpty) //TODO
           | SemanticType.Angular -> (LogNode.intialAngular a)
           | SemanticType.Metric -> (LogNode.intialMetric a)
           | _ -> (LogNode.initialEmpty) //TODO
        )

  

                      
    
    
  let rec generateLevel (lst : List<V3d * Annotation>) (annos : plist<Annotation>) (semApp : SemanticApp) =
    let currentLevel =
      lst
        |> List.map (fun (p,a) -> Annotation.getLevel semApp a)
        |> List.min

    let copyAnnoWith (v : V3d) (aToAdd : (V3d * Annotation)) = 
      let a = 
        (fun (p,a) -> 
          (v, {a with points = PList.ofList [{AnnotationPoint.initial with point    = v
                                                                           selected = false
                                            }]
              }
          )
        ) aToAdd
      a

    let filteredList = 
      lst 
        |> List.filter (fun (p, a) -> (Annotation.getLevel semApp a) = currentLevel)
        |> List.sortBy (fun (p, a) -> (p.Length))

        
    let listWithBorders = 
      List.concat 
            [[copyAnnoWith (V3d.OOO) (List.head filteredList)]; //TODO might lead to problems
              filteredList;
              [(copyAnnoWith (V3d(Double.PositiveInfinity)) (List.last lst))]]

        
    let pwList =
      listWithBorders
        |> List.pairwise

    // TODO performance
//    let minElevation =
//      pwList
//        |> List.map (fun ((up, _), (lp, _)) -> (up.Length,lp.Length)) // TODO make more generic (elevation)
//        |> List.map (fun (x , y) ->  Operators.min x y)
//        |> List.min
//
//    let maxElevation =
//      pwList
//        |> List.map (fun ((up, _), (lp, _)) -> (up.Length,lp.Length)) // TODO make more generic (elevation)
//        |> List.map (fun (x , y) ->  Operators.max x y)
//        |> List.max

    let restAnnos =
      annos
        |> PList.filter (fun a -> (Annotation.getLevel semApp a) <> currentLevel)
        //|> PList.filter (fun a -> (Annotation.elevation a) <= minElevation)
        //|> PList.filter (fun a -> (Annotation.elevation a) >= maxElevation)

    let restSelPoints =
      lst 
        |> List.filter (fun (p, a) -> (Annotation.getLevel semApp a) <> currentLevel)
        //|> List.filter (fun (p, a) -> (Annotation.elevation a) >= minElevation)
        //|> List.filter (fun (p, a) -> (Annotation.elevation a) <= maxElevation)



    seq { // TODO
      for ((p1, a1), (p2, a2)) in pwList do
        // sort pairs
        let (lp, la) = if p1.Length < p2.Length then (p1, a1) else (p2, a2) //TODO refactor
        let (up, ua) = if p1.Length < p2.Length then (p2, a2) else (p1, a1)
        let layerChildren = 
         // let upper = if up.Length > lp.Length then up.Length else lp.Length // could be left out if order can be relied on
         // let lower = if up.Length < lp.Length then up.Length else lp.Length
          let between (a : Annotation) = (lp.Length < (Annotation.elevation a) && (up.Length > (Annotation.elevation a)))
          let layerRestAnnos = 
            restAnnos
              |> PList.filter between
          let layerRestSelPoints = 
            restSelPoints
              |> List.filter (fun (p, a) -> (between a))


          match layerRestSelPoints with
            | []    -> generateNonLevelNodes layerRestAnnos semApp
            | cLst  -> generateLevel layerRestSelPoints layerRestAnnos semApp

        yield LogNode.initialTopLevel (up, ua) (lp, la) layerChildren
    }
    |> PList.ofSeq


  

  let intial (id : string) (lst : List<V3d * Annotation>) (annos : plist<Annotation>) (semApp : SemanticApp): GeologicalLog = {
    id          = id
    nodes       = generateLevel lst annos semApp
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
    alist {
      for n in model.nodes do
        let! dn = (LogNode.view n)
        yield dn
    }

      


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
