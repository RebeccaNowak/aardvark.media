namespace CorrelationDrawing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LogNode =

  open Aardvark.Base
  open Aardvark.UI
  open UtilitiesGUI
  open Aardvark.Base.Incremental

//  let calcElevation (anno : Annotation) =
//    anno.points.Mean (fun x -> x.point.Y)
//    
//  let calcPos (node : LogNode) =
//    {node with pos = (V3d.OOO + V3d.OIO * node.logYPos)}
//
//  //let calcPos' (node : LogNode) (logRange : Rangef) =
//    
//
//  let calcRange (anno : Annotation) =
//    let min = (anno.points.Min (fun x y -> x.point.Y < y.point.Y))
//    let max = (anno.points.Max (fun x y -> x.point.Y > y.point.Y))
//    {Rangef.init with min = min.point.Y
//                      max = max.point.Y}
//                          
//  let recalcRange (node : LogNode) =
//    {node with range =
//                {Rangef.init with min = (calcRange node.lBoundary).min
//                                  max = (calcRange node.uBoundary).max
//                }
//    }

//  let recalcRangeAndSize (node : LogNode) =
//      let range = calcRange node.annotation
//      {node with range = range
//                 size  = (V3d.III + 0.5 * V3d.OIO * (Rangef.calcRange range))}


  //////////////
//  let initial (lBoundary : Annotation)
//             (uBoundary : Annotation) 
//             : LogNode  = {
//    label      = "log node"
//    lBoundary   = lBoundary
//    uBoundary   = uBoundary
//    children    = plist.Empty
//    elevation   = ((calcElevation lBoundary) + (calcElevation uBoundary)) * 0.5
//    range       = {Rangef.init with min = (calcRange lBoundary).min
//                                    max = (calcRange uBoundary).max}  
//
//    logYPos     = 0.0
//    pos         = V3d.OOO
//    size        = V3d.OOO
//  }

  let initialEmpty : LogNode = {
    nodeType    = LogNodeType.Empty
    label       = "log node"
    level       = -1
    lBoundary   = {point = V3d.OOO; anno = (Annotation.initial "-1")}
    uBoundary   = {point = V3d.OOO; anno = (Annotation.initial "-1")}
    children    = plist.Empty
    elevation   = 0.0
    range       = Rangef.init
    logYPos     = 0.0
    pos         = V3d.OOO
    size        = V3d.OOO

  }


  let initialTopLevel 
    ((up, ua) : (V3d * Annotation)) 
    ((lp, la) : (V3d * Annotation)) 
    (children : plist<LogNode>)
    (level    : int) : LogNode = {

    nodeType    = LogNodeType.TopLevel
    label       = "log node"
    level       = level
    lBoundary   = {point = lp; anno = la}
    uBoundary   = {point = up; anno = ua}
    children    = children
    elevation   = (up.Length + lp.Length) * 0.5
    range       = Rangef.init
    logYPos     = 0.0
    pos         = V3d.OOO
    size        = V3d.OOO

  }

  // TODO add level
  let initialHierarchical (anno : Annotation) (annos : plist<Annotation>) =
    {initialEmpty with
      nodeType    = LogNodeType.Hierarchical
      elevation   = Annotation.elevation anno}

  let intialMetric (anno : Annotation) (lower : Border) (upper : Border) =
    {initialEmpty with
      nodeType    = LogNodeType.Metric
      elevation   = Annotation.elevation anno
      lBoundary   = lower
      uBoundary   = upper}


  let intialAngular (anno : Annotation) (lower : Border) (upper : Border)  =
    {initialEmpty with
      nodeType    = LogNodeType.Angular
      elevation   = Annotation.elevation anno
      lBoundary   = lower
      uBoundary   = upper}
  /////////////////////

  let description (model : MLogNode) (semApp : MSemanticApp) = 
    let createDomNode (descString : IMod<string>) =
      div [] [
        Annotation.getColourIcon model.lBoundary.anno semApp
        Annotation.getColourIcon model.uBoundary.anno semApp
        Incremental.text descString
      ]

    let modStr = 
      model.nodeType 
        |> Mod.bind 
          (fun t -> 
            match t with
              | LogNodeType.Hierarchical | LogNodeType.Infinity | LogNodeType.TopLevel ->
                (Mod.map2 (fun (u : V3d) (l : V3d)  -> 
                                sprintf "%.2f-%.2f" l.Length u.Length)
                          model.uBoundary.point model.lBoundary.point)
              | LogNodeType.Angular | LogNodeType.Metric ->
                  model.elevation |> Mod.map (sprintf "%.2f" )
              | LogNodeType.Empty -> Mod.constant "EmptyNode"
          )
      
    createDomNode modStr

  let rec view' (model : MLogNode) (semApp : MSemanticApp) =
//    let description = Incremental.text 
//                        (Mod.map2 (fun (u : V3d) (l : V3d)  -> 
//                                        sprintf "%.2f-%.2f" l.Length u.Length)
//                                  model.uBoundary.point model.lBoundary.point)
    let childrenView = 
      alist {
        for c in model.children do
          let! (v : alist<DomNode<'a>>) = (view' c semApp)
          for it in v do
            yield it
      }
    
    let rval =
      adaptive {
        let isEmpty = AList.isEmpty model.children
        let! (b : bool) = isEmpty
        match b with
          | true  -> 
              return AList.ofList [li [attribute "value" ">"] [(description model semApp)]]
          | false ->                
              return AList.ofList [li [attribute "value" "-"] [(description model semApp)];
                     ul [] [Incremental.li (AttributeMap.ofList [attribute "value" "-"]) childrenView]]
      }
    rval

//  let view (model : MLogNode) =
//
//  //
////    let intoTd (x) = // TODO move to utils
////      adaptive {
////        let! hov = model.uBoundary.anno.hovered
////        match hov with
////          | true -> return td [clazz "center aligned";  style lrPadding] [x]//return td [clazz "center aligned"; style (sprintf "%s;%s" (bgColorStr C4b.Yellow) lrPadding)] [x]
////          | false -> return td [clazz "center aligned";  style lrPadding] [x]
////      }
//      
//
//    let labelText (p : IMod<V3d> ) = 
//      p |> Mod.map (fun v -> sprintf "(%.1f, %.1f, %.1f)" v.X v.Y v.Z)
//    let pToTxt = Mod.map (fun (x : V3d) -> sprintf "%.2f" (x.Length))     
//
//    let append (x : IMod<string>) (str : string) (y : IMod<string>)  = 
//      Mod.map2 (fun a b -> sprintf "%s%s%s" a str b) x y
//    div [] [
//      Incremental.text (append (pToTxt model.uBoundary.point) ", " (pToTxt model.lBoundary.point));
//      Incremental.text (model.children.Content |> Mod.map (fun lst -> sprintf "children: %i" lst.Count))
//
//      //Incremental.text (Mod.map  (sprintf "%s%s") (model.uBoundary.anno.) )
//      ]
     





    

//  let calcPos' (logHeight : float) (lst : List<LogNode>) =
//      let accHeight = lst |> List.sumBy (fun x -> (abs (Rangef.calcRange x.range)))
//      let factor = logHeight / accHeight
//      lst
//        |> List.map (fun (x : LogNode) -> 
//                        {x with size = V3d.IOI + V3d.OIO * (Rangef.calcRange x.range) * factor}
//                    )
//        |> List.scan (fun (x : LogNode) (y : LogNode) -> 
//                        {y with logYPos = x.logYPos + (x.size.Y * 0.5) + (y.size.Y * 0.5)}
//                      ) (dummyInitial) 
//        |> List.tail
//        |> List.map calcPos
//   
//
//  let onlyHierarchicalNodes (semanticApp : SemanticApp) (nodes : List<LogNode>) =
//    nodes
//      |> List.filter (fun (n : LogNode) -> 
//      match (SemanticApp.getSemantic semanticApp n.lBoundary.semanticId) with //assuming one Node contains two annos of same SemanticType
//        | Some s  -> s.semanticType = SemanticType.Hierarchical
//        | None    -> false)


