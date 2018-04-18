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
type SemanticType = Metric = 0 | Angular = 1 | Hierarchical = 2

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
   geometry          : GeometryType
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
type Annotation = {     
    [<NonIncremental;PrimaryKey>]
    id                    : string
    
    [<NonIncremental>]
    geometry              : GeometryType

    [<NonIncremental>]
    projection            : Projection

    semanticId            : string
    points                : plist<V3d>
    segments              : plist<plist<V3d>> //list<Segment>
    visible               : bool
    text                  : string
    overrideGeometryType  : option<GeometryType>
    overrideStyle         : option<Style>
    overrideLevel         : option<int>
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
    annotations : plist<Annotation>
}


[<DomainType>]
type LogNode = {
    annotation  : Annotation

    elevation   : float
    [<NonIncremental>]
    pos         : V3d
    [<NonIncremental>]
    size        : V3d

    colour      : C4b //remove
    nodeType    : SemanticType //remove
    index       : int
}

[<DomainType>]
type GeologicalLog = {
    [<NonIncremental;PrimaryKey>]
    id        : string
    nodes     : plist<LogNode>
    borders   : plist<Border>
    range     : V2d //?
    camera    : CameraControllerState
}

type AnnotationParameters = {Point:V3d;semanticId:string}

[<DomainType>]
type CorrelationDrawingModel = {
    draw                : bool 
    hoverPosition       : option<Trafo3d>
    working             : option<Annotation>
    projection          : Projection //TODO move to semantic
    geometry            : GeometryType //TODO move to semantic
//    semantics           : hmap<string, Semantic>
//    semanticsList       : DropdownList<Semantic>
    //selectedSemantic    : Semantic
    selectedAnnotation  : option<string>
    annotations         : plist<Annotation>
    exportPath          : string
    log                 : GeologicalLog
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

        cameraState : CameraControllerState

        cullMode    : CullMode
        fill        : bool

        dockConfig  : DockConfig

        drawingApp  : CorrelationAppModel
        semanticApp : SemanticApp
        log         : GeologicalLog
    }