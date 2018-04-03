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


[<DomainType>]
type Semantic = {
   [<NonIncremental;PrimaryKey>]
   id                : string

   [<NonIncremental>]
   timestamp         : string

   disabled          : bool
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
 }

[<DomainType>]
type Annotation = {     
    [<NonIncremental;PrimaryKey>]
    id                : string
    
    [<NonIncremental>]
    geometry          : GeometryType

    [<NonIncremental>]
    projection        : Projection

    semanticId              : string
    points                : plist<V3d>
    segments              : plist<plist<V3d>> //list<Segment>
    visible               : bool
    text                  : string
    overrideGeometryType  : option<GeometryType>
    overrideStyle         : option<Style>
    overrideLevel         : option<int>
}

[<DomainType>]
type Border = {
    annotations : plist<Annotation>
}

[<DomainType>]
type LogModel = {
    [<NonIncremental;PrimaryKey>]
    id      : string
    //borders : alist<Annotation>
    range   : V2d //?

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
    }