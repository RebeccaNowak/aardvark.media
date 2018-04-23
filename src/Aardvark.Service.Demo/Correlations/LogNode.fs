namespace CorrelationDrawing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LogNode =

  open Aardvark.Base

  let calcElevation (anno : Annotation) =
    anno.points.Mean (fun x -> x.Y)
    
  let calcPos (node : LogNode) =
    (V3d.OOO + V3d.OIO * (float node.index))

  let recalcPos (node : LogNode) =
    {node with pos = calcPos node}

  let recalcPos' (node : LogNode) =
    {node with pos = (V3d.OOO + V3d.OIO * node.logYPos)}

  //let calcPos' (node : LogNode) (logRange : Rangef) =
    

  let calcRange (anno : Annotation) =
    let min = (anno.points.Min (fun x y -> x.Y < y.Y))
    let max = (anno.points.Max (fun x y -> x.Y > y.Y))
    {Rangef.init with min = min.Y
                      max = max.Y}
                          
  let recalcRange (node : LogNode) =
    {node with range = calcRange node.annotation}

  let recalcRangeAndSize (node : LogNode) =
      let range = calcRange node.annotation
      {node with range = range
                 size  = (V3d.III + 0.5 * V3d.OIO * (Rangef.calcRange range))}

   
  let intial (anno : Annotation) (ind : int) : LogNode  = {
    annotation  = anno
    children    = plist.Empty
    elevation   = calcElevation anno
    range       = calcRange anno
    logYPos     = 0.0
    pos         = (V3d.OOO + V3d.OIO * (float ind))
    size        = V3d.OOO
    index       = ind 
  }
