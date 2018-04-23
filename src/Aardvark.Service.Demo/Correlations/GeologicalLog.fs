namespace CorrelationDrawing

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

  let intial id : GeologicalLog = {
    id          = id
    nodes       = PList.empty
    borders     = PList.empty
    range       = Rangef.init
    camera      = 
      { ArcBallController.initial with 
                     view = CameraView.lookAt (2.0 * V3d.III) V3d.Zero V3d.OOI}    
  }

  /// HELPER FUNCTIONS

  let addNode (model : GeologicalLog) (anno : Annotation) =
    let newNode = LogNode.intial anno 0 
    let newNodes = 
      (model.nodes.Append newNode)
        |> PList.toList
        |> List.sortBy (fun (x : LogNode) -> x.elevation)
        |> PList.ofList
        |> PList.mapiInt
        |> PList.map (fun (x, i) -> {x with index = i}) 
        |> PList.map LogNode.recalcRangeAndSize
        
    let calcYPos (lst : List<LogNode>) =
      let maxLogHeight = 10.0
      let accHeight = lst |> List.sumBy (fun x -> (abs (Rangef.calcRange x.range)))
      let factor = maxLogHeight / accHeight
      let lst = 
        lst
          |> List.map (fun (x : LogNode) -> 
                          {x with size = V3d.IOI + V3d.OIO * (Rangef.calcRange x.range) * factor}
                      )
      lst
        |> List.scan (fun (x : LogNode) (y : LogNode) -> 
                        {y with logYPos = x.logYPos + (x.size.Y * 0.5) + (y.size.Y * 0.5)}
                     ) (LogNode.intial anno 0) 
        |> List.tail

    let nodesWithPositions = 
      let lst = 
        newNodes
          |> PList.toList
          |> List.sortBy (fun x -> x.elevation)
      lst  
//        |> List.scan (fun (x : LogNode) (y : LogNode) -> 
//                        {y with logYPos = x.logYPos + (Rangef.calcRange y.range)}
//                     ) (LogNode.intial anno 0)
//        |> List.tail
        |> calcYPos
        |> PList.ofList
        |> PList.map LogNode.recalcPos'
//    let logHeight =
//          newNodes 
//            |> PList.map (fun x -> Rangef.calcRange x.range)
//            |> PList.toList
//            |> List.sum

//      let max =  
//          newNodes 
//            |> PList.map (fun x -> x.range.max)
//            |> PList.toList
//            |> List.max

//      let newnewNodes = 
//        newNodes
//          |> PList.map LogNode.recalcPos
            
      //{Rangef.init with min = min; max = max}
    
    {model with nodes = nodesWithPositions}  


      
                         


  type Action =
      | CameraMessage             of ArcBallController.Message    
      | AddNode                   of Annotation
      

  let update (model : GeologicalLog) (msg : Action) =
    match msg with 
      | CameraMessage m -> 
          {model with camera = ArcBallController.update model.camera m}
      | AddNode anno ->
          addNode model anno





  let inline (==>) a b = Attributes.attribute a b

//  let sortAnnosbyElevation (annos : alist<MAnnotation>) =
//    AList.sortBy (Annotation.getAvgElevation) annos 

  let createAnnoBox (node : MLogNode) (colour : IMod<C4b>) =
    
   
    let foo = C4b(0.0, 0.0, 0.0, 0.5)
    let box =
        let c = colour |> Mod.map ( fun col -> C4b(col.R, col.G, col.B, foo.A)) 
        let b = Mod.map2 (fun pos size -> (Box3d.FromCenterAndSize(pos, size))) node.pos node.size
        let lbl = Mod.map2 (fun (x : V3d) (y : float) -> sprintf "%f / %f" x.Y y) node.size node.logYPos
        let lblSg = (UtilitiesRendering.makeLblSg' lbl node.pos)

        (Sg.box c b)                   
          |> Sg.shader {
              do! DefaultSurfaces.trafo
              do! DefaultSurfaces.vertexColor
              do! DefaultSurfaces.simpleLighting
          } 

      |> Sg.andAlso lblSg  
          //|> Sg.noEvents
          //|> Sg.translate' (Mod.constant (V3d.OIO * pos))
          //|> Sg.billboard
      

    box



  let sg (model       : MGeologicalLog)
         (annos       : alist<MAnnotation>) 
         (semanticApp : MSemanticApp) 
         (camView     : IMod<CameraView>) = //(pos : IMod<V3d>) =

    alist {
      for node in model.nodes do
        yield createAnnoBox node (Annotation.getColor' node.annotation semanticApp) 
    }
    |> AList.toASet
    |> Sg.set
    


    







  let view (runtime : IRuntime) (model : MGeologicalLog) =

    let domNode =
      let attributes =
        AttributeMap.ofList [
              attribute "style" "width:100%; height: 100%;border: 1px solid black"
              clazz "svgRoot"; 
        ]

      ArcBallController.controlledControl 
        model.camera
        CameraMessage 
        (Mod.constant (Frustum.perspective 60.0 0.1 100.0 1.0))
        (AttributeMap.ofList [
          attribute "style" "width:100%; height: 100%;border: 1px solid black"
          clazz "svgRoot"; 
        ])
        (
          //sg model.camera.view (Mod.constant V3d.OOO)
          renderLblTextureQuad runtime model.camera.view
        )

    body [clazz "ui"; style "background: #1B1C1E; width: 100%; height:100%; overflow: auto;"] [
      div [] [
        domNode
      ]
    ]
    
    ////////////

    //            let heightList = 
//              annos 
//                |> AList.map (fun x -> Annotation.getRange x)
//                |> AList.toList
//                
//
//            let annoKeys =
//              annos 
//                |> AList.map (fun x -> x.id)
//                |> AList.toList
//            
//            let f (i : int) (value : IMod<float>) (valLst : List<IMod<float>>) =
//              value 
//                |> Mod.map (fun (x : float) ->
//                              match x with
//                                | h when i < 1 -> x
//                                | h when i >= 1 -> x + (Mod.force (valLst.Item (i - 1)))
//                                | _ -> 0.0)
//
//            let accHeightList = (heightList 
//                                  |> List.mapi (fun i x -> f i x heightList))
//
//
//            let heightMap = 
//              List.zip annoKeys accHeightList
//              |> AMap.ofList
//
//            //let minMax = Annotation.getMinMaxElevation annos
//
//
//            let annoSet = ASet.ofAList annos
//         
////
//            let sgSet = 
//              aset {
//                for anno in annoSet do
//                  let pos = AMap.find anno.id heightMap
//                  let tmp = (Annotation.getRange anno) |> Mod.map 
//                              (fun el -> (createAnnoBox anno semanticApp camView (pos |> Mod.force |> Mod.force)))
//                  yield tmp |> Sg.dynamic
//              }

//            sgSet |> Sg.set

//    (Mod.map sgSetLayers (AList.isEmpty annos))
//      |> Sg.dynamic



///////////////////////


//  let createAnnoBox (anno : MAnnotation) 
//                    (semanticApp : MSemanticApp) 
//                    (view : IMod<CameraView>) 
//                    (pos : float) = 
////    let range =
////      (Annotation.getRange anno) 
////    let boxPos = V3d.IOI + (V3d.OIO * pos)
////    Sg.box
////      (Annotation.getColor' anno semanticApp)  
////      (range
////        |> (Mod.map 
////              (fun x -> 
////                (Box3d.FromCenterAndSize(boxPos, V3d.OIO * 2.0))
////              )
////            )
////      )
//    Sg.box (Annotation.getColor' anno semanticApp) 
//           (Mod.constant (Box3d.FromCenterAndSize(V3d.III, V3d.III * 5.0)))
//      |> Sg.shader {
//          do! DefaultSurfaces.trafo
//          do! DefaultSurfaces.vertexColor
//          do! DefaultSurfaces.simpleLighting
//      }         
//          //|> Sg.trafo scale
//      //|> Sg.trafo ( view |> Mod.map ( fun v -> Trafo3d.RotateInto(V3d.OOI, v.Sky) ) )
//      |> Sg.noEvents
//      //|> Sg.translate' (Mod.constant (V3d.OIO * pos))
//      |> Sg.billboard
