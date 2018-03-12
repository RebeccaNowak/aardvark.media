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

type Projection = Linear = 0 | Viewpoint = 1 | Sky = 2
type GeometryType = Point = 0 | Line = 1 | Polyline = 2 | Polygon = 3 | DnS = 4 | Undefined = 5
type SemanticType = Metric = 0 | Angular = 1 | Hierarchical = 2

[<DomainType>]
type Style = {
    color : ColorInput
    thickness : NumericInput
 } 

[<DomainType>]
type RenderingParameters = {
    fillMode : FillMode
    cullMode : CullMode
}   
    
module RenderingPars =
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    let initial : RenderingParameters = {
        fillMode = FillMode.Fill
        cullMode = CullMode.None
    }


[<DomainType>]
type Semantic = {
   [<NonIncremental;PrimaryKey>]
   id                : string

   disabled          : bool
   label             : TextInput
   size              : double
   style             : Style
   geometry          : GeometryType
   semanticType      : SemanticType
 }

[<DomainType>]
type Annotation = {     
    [<NonIncremental;PrimaryKey>]
    id                : string
    
    [<NonIncremental>]
    geometry : GeometryType

    [<NonIncremental>]
    projection : Projection

    semanticId : string
    points : plist<V3d>
    segments : plist<plist<V3d>> //list<Segment>
    visible : bool
    text : string

    // semDropdown : DropdownList<Semantic>
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

[<DomainType>]
type CorrelationDrawingModel = {
    draw                : bool 
    hoverPosition       : option<Trafo3d>
    working             : option<Annotation>
    projection          : Projection //TODO move to semantic
    geometry            : GeometryType //TODO move to semantic
    semantics           : hmap<string, Semantic>
    semanticsList       : DropdownList<Semantic>
    selectedSemantic    : string
    selectedAnnotation  : option<string>
    annotations         : plist<Annotation>
    exportPath          : string
}

[<DomainType>]
type CorrelationAppModel = {
    camera           : CameraControllerState
    rendering        : RenderingParameters
    drawing          : CorrelationDrawingModel

    [<TreatAsValue>]
    history          : Option<CorrelationAppModel> 

    [<TreatAsValue>]
    future           : Option<CorrelationAppModel>     
}