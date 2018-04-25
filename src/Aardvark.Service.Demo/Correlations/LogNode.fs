namespace CorrelationDrawing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LogNode =

  open Aardvark.Base

  let calcElevation (anno : Annotation) =
    anno.points.Mean (fun x -> x.Y)
    
  let calcPos (node : LogNode) =
    {node with pos = (V3d.OOO + V3d.OIO * node.logYPos)}

  //let calcPos' (node : LogNode) (logRange : Rangef) =
    

  let calcRange (anno : Annotation) =
    let min = (anno.points.Min (fun x y -> x.Y < y.Y))
    let max = (anno.points.Max (fun x y -> x.Y > y.Y))
    {Rangef.init with min = min.Y
                      max = max.Y}
                          
  let recalcRange (node : LogNode) =
    {node with range =
                {Rangef.init with min = (calcRange node.lBoundary).min
                                  max = (calcRange node.uBoundary).max
                }
    }

//  let recalcRangeAndSize (node : LogNode) =
//      let range = calcRange node.annotation
//      {node with range = range
//                 size  = (V3d.III + 0.5 * V3d.OIO * (Rangef.calcRange range))}


  //////////////
  let initial (lBoundary : Annotation)
             (uBoundary : Annotation) 
             : LogNode  = {
    lBoundary   = lBoundary
    uBoundary   = uBoundary
    children    = plist.Empty
    elevation   = ((calcElevation lBoundary) + (calcElevation uBoundary)) * 0.5
    range       = {Rangef.init with min = (calcRange lBoundary).min
                                    max = (calcRange uBoundary).max}  

    logYPos     = 0.0
    pos         = V3d.OOO
    size        = V3d.OOO
  }

  let dummyInitial : LogNode = {
    lBoundary  = Annotation.initial "-1"
    uBoundary  = Annotation.initial "-1"
    children    = plist.Empty
    elevation   = 0.0
    range       = Rangef.init
    logYPos     = 0.0
    pos         = V3d.OOO
    size        = V3d.OOO

  }
  /////////////////////

  let calcPos' (logHeight : float) (lst : List<LogNode>) =
      let accHeight = lst |> List.sumBy (fun x -> (abs (Rangef.calcRange x.range)))
      let factor = logHeight / accHeight
      lst
        |> List.map (fun (x : LogNode) -> 
                        {x with size = V3d.IOI + V3d.OIO * (Rangef.calcRange x.range) * factor}
                    )
        |> List.scan (fun (x : LogNode) (y : LogNode) -> 
                        {y with logYPos = x.logYPos + (x.size.Y * 0.5) + (y.size.Y * 0.5)}
                      ) (dummyInitial) 
        |> List.tail
        |> List.map calcPos
   

  let onlyHierarchicalNodes (semanticApp : SemanticApp) (nodes : List<LogNode>) =
    nodes
      |> List.filter (fun (n : LogNode) -> 
      match (SemanticApp.getSemantic semanticApp n.lBoundary.semanticId) with //assuming one Node contains two annos of same SemanticType
        | Some s  -> s.semanticType = SemanticType.Hierarchical
        | None    -> false)


