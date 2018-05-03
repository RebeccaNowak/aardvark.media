namespace CorrelationDrawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
//open Aardvark.UI.Mutable
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application


/// GUI

[<DomainType>]
type TextInput = {
    text      : string
    disabled  : bool
    bgColor   : C4b
    size      : option<int>
 } 

 [<DomainType>]
 type DropdownList<'a> = {
   valueList          : plist<'a>
   selected           : option<'a>
   color              : C4b
   searchable         : bool
   //changeFunction     : (option<'a> -> 'msg) @Thomas proper way?
   //labelFunction      : ('a -> IMod<string>)
   //getIsSelected      : ('a -> IMod<bool>) 
 } 

/// END GUI


/// CORRELATION PANELS

type Projection   = Linear = 0 | Viewpoint = 1 | Sky = 2
type GeometryType = Point = 0 | Line = 1 | Polyline = 2 | Polygon = 3 | DnS = 4 | Undefined = 5
type SemanticType = Metric = 0 | Angular = 1 | Hierarchical = 2 | Undefined = 3
type Rangef = {
  min     : float
  max     : float
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rangef =
  let init : Rangef = {
      min     = 0.0
      max     = 0.0
    }

  let calcRange (r : Rangef) =
    r.max - r.min

[<DomainType>]
type Style = {
    color     : ColorInput
    thickness : NumericInput
 } 

[<DomainType>]
type RenderingParameters = {
    fillMode : FillMode
    cullMode : CullMode
}   
    
module RenderingPars =
    let initial : RenderingParameters = {
        fillMode = FillMode.Fill
        cullMode = CullMode.None
    }

type SemanticState = New | Edit | Display

[<DomainType>]
type Semantic = {
   [<NonIncremental;PrimaryKey>]
   id                : string

   [<NonIncremental>]
   timestamp         : string

   state             : SemanticState
   label             : TextInput
   size              : double
   style             : Style
   semanticType      : SemanticType
   level             : int
 }

 type SemanticsSortingOption = Label = 0 | Level = 1 | GeometryType = 2 | SemanticType = 3 | Id = 4 | Timestamp = 5

 [<DomainType>]
 type SemanticApp = {
   semantics           : hmap<string, Semantic>
   semanticsList       : plist<Semantic>
   selectedSemantic    : string
   sortBy              : SemanticsSortingOption
   creatingNew         : bool
 }

[<DomainType>]
type AnnotationPoint = {
  point     : V3d
  selected  : bool
}




[<DomainType>]
type Annotation = {     
    [<NonIncremental;PrimaryKey>]
    id                    : string
    
    [<NonIncremental>]
    geometry              : GeometryType

    [<NonIncremental>]
    projection            : Projection

    [<NonIncremental>]
    semanticType          : SemanticType

    selected              : bool
    hovered               : bool

    semanticId            : string
    points                : plist<AnnotationPoint>
    segments              : plist<plist<V3d>> //list<Segment>
    visible               : bool
    text                  : string
    overrideStyle         : option<Style>
    overrideLevel         : option<int>
    grainSize             : float
}

[<DomainType>]
type Horizon = {
  [<NonIncremental;PrimaryKey>]
  id          : string

  annotation  : Annotation
  children    : plist<Annotation>


}


[<DomainType>]
type Border = {
    anno      : Annotation
    point     : V3d
}

type LogNodeType  = TopLevel | Hierarchical | Metric | Angular | Infinity | Empty

[<DomainType>]
type LogNode = {
  // TODO add id
    
    label       : string
    nodeType    : LogNodeType

    lBoundary   : Border
    uBoundary   : Border
    children    : plist<LogNode>

    elevation   : float
    range       : Rangef
    logYPos     : float

    pos         : V3d
    size        : V3d
}

[<DomainType>]
type GeologicalLog = {
    [<NonIncremental;PrimaryKey>]
    id          : string
    annoPoints  : list<(V3d * Annotation)>
    nodes       : plist<LogNode>
    range       : Rangef //?
    camera      : CameraControllerState
}

[<DomainType>]
type CorrelationPlotApp = {
   logs                : plist<GeologicalLog>
   working             : list<(V3d * Annotation)>
   selectedLog         : option<string>
   creatingNew         : bool
}

type AnnotationParameters = {Point:V3d;semanticId:string}

[<DomainType>]
type CorrelationDrawingModel = {
    draw                : bool 
    hoverPosition       : option<Trafo3d>
    working             : option<Annotation>
    projection          : Projection //TODO move to semantic
    geometry            : GeometryType //TODO move to semantic
    selectedAnnotation  : option<string>
    annotations         : plist<Annotation>
    exportPath          : string
}



[<DomainType>]
type CorrelationAppModel = {
    camera           : CameraControllerState
    rendering        : RenderingParameters
    drawing          : CorrelationDrawingModel 
}

[<DomainType>]
type Pages = 
    {
        [<NonIncremental>]
        past        : Option<Pages>

        [<NonIncremental>]
        future      : Option<Pages>

//        cameraState : CameraControllerState

        cullMode    : CullMode
        fill        : bool

        dockConfig  : DockConfig

        drawingApp  : CorrelationAppModel
        semanticApp : SemanticApp
        corrPlotApp : CorrelationPlotApp
    }