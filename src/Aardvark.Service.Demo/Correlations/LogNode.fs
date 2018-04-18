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

   
  let intial (anno : Annotation) (ind : int) : LogNode  = {
    annotation  = anno
    elevation   = calcElevation anno
    pos         = (V3d.OOO + V3d.OIO * (float ind))
    size        = V3d.III
    colour      = C4b.Red
    nodeType    = SemanticType.Hierarchical
    index       = ind 
  }
