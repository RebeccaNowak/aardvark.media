namespace CorrelationDrawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open CorrelationDrawing

[<AutoOpen>]
module Mutable =

    
    
    type MTextInput(__initial : CorrelationDrawing.TextInput) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.TextInput> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.TextInput>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.TextInput>
        let _text = ResetMod.Create(__initial.text)
        let _disabled = ResetMod.Create(__initial.disabled)
        let _bgColor = ResetMod.Create(__initial.bgColor)
        let _size = MOption.Create(__initial.size)
        
        member x.text = _text :> IMod<_>
        member x.disabled = _disabled :> IMod<_>
        member x.bgColor = _bgColor :> IMod<_>
        member x.size = _size :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.TextInput) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_text,v.text)
                ResetMod.Update(_disabled,v.disabled)
                ResetMod.Update(_bgColor,v.bgColor)
                MOption.Update(_size, v.size)
                
        
        static member Create(__initial : CorrelationDrawing.TextInput) : MTextInput = MTextInput(__initial)
        static member Update(m : MTextInput, v : CorrelationDrawing.TextInput) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.TextInput> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TextInput =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let text =
                { new Lens<CorrelationDrawing.TextInput, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.text
                    override x.Set(r,v) = { r with text = v }
                    override x.Update(r,f) = { r with text = f r.text }
                }
            let disabled =
                { new Lens<CorrelationDrawing.TextInput, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.disabled
                    override x.Set(r,v) = { r with disabled = v }
                    override x.Update(r,f) = { r with disabled = f r.disabled }
                }
            let bgColor =
                { new Lens<CorrelationDrawing.TextInput, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.bgColor
                    override x.Set(r,v) = { r with bgColor = v }
                    override x.Update(r,f) = { r with bgColor = f r.bgColor }
                }
            let size =
                { new Lens<CorrelationDrawing.TextInput, Microsoft.FSharp.Core.option<Microsoft.FSharp.Core.int>>() with
                    override x.Get(r) = r.size
                    override x.Set(r,v) = { r with size = v }
                    override x.Update(r,f) = { r with size = f r.size }
                }
    [<AbstractClass; StructuredFormatDisplay("{AsString}")>]
    type MDropdownList<'va,'na>() = 
        abstract member valueList : Aardvark.Base.Incremental.alist<'na>
        abstract member selected : Aardvark.Base.Incremental.IMod<Microsoft.FSharp.Core.option<'na>>
        abstract member color : Aardvark.Base.Incremental.IMod<Aardvark.Base.C4b>
        abstract member searchable : Aardvark.Base.Incremental.IMod<Microsoft.FSharp.Core.bool>
        abstract member AsString : string
    
    
    and private MDropdownListD<'a,'ma,'va>(__initial : CorrelationDrawing.DropdownList<'a>, __ainit : 'a -> 'ma, __aupdate : 'ma * 'a -> unit, __aview : 'ma -> 'va) =
        inherit MDropdownList<'va,'va>()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.DropdownList<'a>> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.DropdownList<'a>>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.DropdownList<'a>>
        let _valueList = MList.Create(__initial.valueList, (fun v -> __ainit(v)), (fun (m,v) -> __aupdate(m, v)), (fun v -> __aview(v)))
        let _selected = MOption.Create(__initial.selected, (fun v -> __ainit(v)), (fun (m,v) -> __aupdate(m, v)), (fun v -> __aview(v)))
        let _color = ResetMod.Create(__initial.color)
        let _searchable = ResetMod.Create(__initial.searchable)
        
        override x.valueList = _valueList :> alist<_>
        override x.selected = _selected :> IMod<_>
        override x.color = _color :> IMod<_>
        override x.searchable = _searchable :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.DropdownList<'a>) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_valueList, v.valueList)
                MOption.Update(_selected, v.selected)
                ResetMod.Update(_color,v.color)
                ResetMod.Update(_searchable,v.searchable)
                
        
        static member Update(m : MDropdownListD<'a,'ma,'va>, v : CorrelationDrawing.DropdownList<'a>) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        override x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.DropdownList<'a>> with
            member x.Update v = x.Update v
    
    and private MDropdownListV<'a>(__initial : CorrelationDrawing.DropdownList<'a>) =
        inherit MDropdownList<IMod<'a>,'a>()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.DropdownList<'a>> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.DropdownList<'a>>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.DropdownList<'a>>
        let _valueList = MList.Create(__initial.valueList)
        let _selected = MOption.Create(__initial.selected)
        let _color = ResetMod.Create(__initial.color)
        let _searchable = ResetMod.Create(__initial.searchable)
        
        override x.valueList = _valueList :> alist<_>
        override x.selected = _selected :> IMod<_>
        override x.color = _color :> IMod<_>
        override x.searchable = _searchable :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.DropdownList<'a>) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_valueList, v.valueList)
                MOption.Update(_selected, v.selected)
                ResetMod.Update(_color,v.color)
                ResetMod.Update(_searchable,v.searchable)
                
        
        static member Update(m : MDropdownListV<'a>, v : CorrelationDrawing.DropdownList<'a>) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        override x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.DropdownList<'a>> with
            member x.Update v = x.Update v
    
    and [<AbstractClass; Sealed>] MDropdownList private() =
        static member Create<'a,'ma,'va>(__initial : CorrelationDrawing.DropdownList<'a>, __ainit : 'a -> 'ma, __aupdate : 'ma * 'a -> unit, __aview : 'ma -> 'va) : MDropdownList<'va,'va> = MDropdownListD<'a,'ma,'va>(__initial, __ainit, __aupdate, __aview) :> MDropdownList<'va,'va>
        static member Create<'a>(__initial : CorrelationDrawing.DropdownList<'a>) : MDropdownList<IMod<'a>,'a> = MDropdownListV<'a>(__initial) :> MDropdownList<IMod<'a>,'a>
        static member Update<'a,'xva,'xna>(m : MDropdownList<'xva,'xna>, v : CorrelationDrawing.DropdownList<'a>) : unit = 
            match m :> obj with
            | :? IUpdatable<CorrelationDrawing.DropdownList<'a>> as m -> m.Update(v)
            | _ -> failwith "cannot update"
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DropdownList =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let valueList<'a> =
                { new Lens<CorrelationDrawing.DropdownList<'a>, Aardvark.Base.plist<'a>>() with
                    override x.Get(r) = r.valueList
                    override x.Set(r,v) = { r with valueList = v }
                    override x.Update(r,f) = { r with valueList = f r.valueList }
                }
            let selected<'a> =
                { new Lens<CorrelationDrawing.DropdownList<'a>, Microsoft.FSharp.Core.option<'a>>() with
                    override x.Get(r) = r.selected
                    override x.Set(r,v) = { r with selected = v }
                    override x.Update(r,f) = { r with selected = f r.selected }
                }
            let color<'a> =
                { new Lens<CorrelationDrawing.DropdownList<'a>, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.color
                    override x.Set(r,v) = { r with color = v }
                    override x.Update(r,f) = { r with color = f r.color }
                }
            let searchable<'a> =
                { new Lens<CorrelationDrawing.DropdownList<'a>, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.searchable
                    override x.Set(r,v) = { r with searchable = v }
                    override x.Update(r,f) = { r with searchable = f r.searchable }
                }
    
    
    type MStyle(__initial : CorrelationDrawing.Style) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Style> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.Style>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Style>
        let _color = Aardvark.UI.Mutable.MColorInput.Create(__initial.color)
        let _thickness = Aardvark.UI.Mutable.MNumericInput.Create(__initial.thickness)
        
        member x.color = _color
        member x.thickness = _thickness
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.Style) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Mutable.MColorInput.Update(_color, v.color)
                Aardvark.UI.Mutable.MNumericInput.Update(_thickness, v.thickness)
                
        
        static member Create(__initial : CorrelationDrawing.Style) : MStyle = MStyle(__initial)
        static member Update(m : MStyle, v : CorrelationDrawing.Style) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.Style> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Style =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let color =
                { new Lens<CorrelationDrawing.Style, Aardvark.UI.ColorInput>() with
                    override x.Get(r) = r.color
                    override x.Set(r,v) = { r with color = v }
                    override x.Update(r,f) = { r with color = f r.color }
                }
            let thickness =
                { new Lens<CorrelationDrawing.Style, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.thickness
                    override x.Set(r,v) = { r with thickness = v }
                    override x.Update(r,f) = { r with thickness = f r.thickness }
                }
    
    
    type MRenderingParameters(__initial : CorrelationDrawing.RenderingParameters) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.RenderingParameters> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.RenderingParameters>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.RenderingParameters>
        let _fillMode = ResetMod.Create(__initial.fillMode)
        let _cullMode = ResetMod.Create(__initial.cullMode)
        
        member x.fillMode = _fillMode :> IMod<_>
        member x.cullMode = _cullMode :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.RenderingParameters) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_fillMode,v.fillMode)
                ResetMod.Update(_cullMode,v.cullMode)
                
        
        static member Create(__initial : CorrelationDrawing.RenderingParameters) : MRenderingParameters = MRenderingParameters(__initial)
        static member Update(m : MRenderingParameters, v : CorrelationDrawing.RenderingParameters) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.RenderingParameters> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RenderingParameters =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let fillMode =
                { new Lens<CorrelationDrawing.RenderingParameters, Aardvark.Base.Rendering.FillMode>() with
                    override x.Get(r) = r.fillMode
                    override x.Set(r,v) = { r with fillMode = v }
                    override x.Update(r,f) = { r with fillMode = f r.fillMode }
                }
            let cullMode =
                { new Lens<CorrelationDrawing.RenderingParameters, Aardvark.Base.Rendering.CullMode>() with
                    override x.Get(r) = r.cullMode
                    override x.Set(r,v) = { r with cullMode = v }
                    override x.Update(r,f) = { r with cullMode = f r.cullMode }
                }
    
    
    type MSemantic(__initial : CorrelationDrawing.Semantic) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Semantic> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.Semantic>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Semantic>
        let _state = ResetMod.Create(__initial.state)
        let _label = MTextInput.Create(__initial.label)
        let _size = ResetMod.Create(__initial.size)
        let _style = MStyle.Create(__initial.style)
        let _semanticType = ResetMod.Create(__initial.semanticType)
        let _level = ResetMod.Create(__initial.level)
        
        member x.id = __current.Value.id
        member x.timestamp = __current.Value.timestamp
        member x.state = _state :> IMod<_>
        member x.label = _label
        member x.size = _size :> IMod<_>
        member x.style = _style
        member x.semanticType = _semanticType :> IMod<_>
        member x.level = _level :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.Semantic) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_state,v.state)
                MTextInput.Update(_label, v.label)
                ResetMod.Update(_size,v.size)
                MStyle.Update(_style, v.style)
                ResetMod.Update(_semanticType,v.semanticType)
                ResetMod.Update(_level,v.level)
                
        
        static member Create(__initial : CorrelationDrawing.Semantic) : MSemantic = MSemantic(__initial)
        static member Update(m : MSemantic, v : CorrelationDrawing.Semantic) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.Semantic> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Semantic =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<CorrelationDrawing.Semantic, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let timestamp =
                { new Lens<CorrelationDrawing.Semantic, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.timestamp
                    override x.Set(r,v) = { r with timestamp = v }
                    override x.Update(r,f) = { r with timestamp = f r.timestamp }
                }
            let state =
                { new Lens<CorrelationDrawing.Semantic, CorrelationDrawing.SemanticState>() with
                    override x.Get(r) = r.state
                    override x.Set(r,v) = { r with state = v }
                    override x.Update(r,f) = { r with state = f r.state }
                }
            let label =
                { new Lens<CorrelationDrawing.Semantic, CorrelationDrawing.TextInput>() with
                    override x.Get(r) = r.label
                    override x.Set(r,v) = { r with label = v }
                    override x.Update(r,f) = { r with label = f r.label }
                }
            let size =
                { new Lens<CorrelationDrawing.Semantic, Microsoft.FSharp.Core.double>() with
                    override x.Get(r) = r.size
                    override x.Set(r,v) = { r with size = v }
                    override x.Update(r,f) = { r with size = f r.size }
                }
            let style =
                { new Lens<CorrelationDrawing.Semantic, CorrelationDrawing.Style>() with
                    override x.Get(r) = r.style
                    override x.Set(r,v) = { r with style = v }
                    override x.Update(r,f) = { r with style = f r.style }
                }
            let semanticType =
                { new Lens<CorrelationDrawing.Semantic, CorrelationDrawing.SemanticType>() with
                    override x.Get(r) = r.semanticType
                    override x.Set(r,v) = { r with semanticType = v }
                    override x.Update(r,f) = { r with semanticType = f r.semanticType }
                }
            let level =
                { new Lens<CorrelationDrawing.Semantic, Microsoft.FSharp.Core.int>() with
                    override x.Get(r) = r.level
                    override x.Set(r,v) = { r with level = v }
                    override x.Update(r,f) = { r with level = f r.level }
                }
    
    
    type MSemanticApp(__initial : CorrelationDrawing.SemanticApp) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.SemanticApp> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.SemanticApp>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.SemanticApp>
        let _semantics = MMap.Create(__initial.semantics, (fun v -> MSemantic.Create(v)), (fun (m,v) -> MSemantic.Update(m, v)), (fun v -> v))
        let _semanticsList = MList.Create(__initial.semanticsList, (fun v -> MSemantic.Create(v)), (fun (m,v) -> MSemantic.Update(m, v)), (fun v -> v))
        let _selectedSemantic = ResetMod.Create(__initial.selectedSemantic)
        let _sortBy = ResetMod.Create(__initial.sortBy)
        let _creatingNew = ResetMod.Create(__initial.creatingNew)
        
        member x.semantics = _semantics :> amap<_,_>
        member x.semanticsList = _semanticsList :> alist<_>
        member x.selectedSemantic = _selectedSemantic :> IMod<_>
        member x.sortBy = _sortBy :> IMod<_>
        member x.creatingNew = _creatingNew :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.SemanticApp) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MMap.Update(_semantics, v.semantics)
                MList.Update(_semanticsList, v.semanticsList)
                ResetMod.Update(_selectedSemantic,v.selectedSemantic)
                ResetMod.Update(_sortBy,v.sortBy)
                ResetMod.Update(_creatingNew,v.creatingNew)
                
        
        static member Create(__initial : CorrelationDrawing.SemanticApp) : MSemanticApp = MSemanticApp(__initial)
        static member Update(m : MSemanticApp, v : CorrelationDrawing.SemanticApp) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.SemanticApp> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SemanticApp =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let semantics =
                { new Lens<CorrelationDrawing.SemanticApp, Aardvark.Base.hmap<Microsoft.FSharp.Core.string,CorrelationDrawing.Semantic>>() with
                    override x.Get(r) = r.semantics
                    override x.Set(r,v) = { r with semantics = v }
                    override x.Update(r,f) = { r with semantics = f r.semantics }
                }
            let semanticsList =
                { new Lens<CorrelationDrawing.SemanticApp, Aardvark.Base.plist<CorrelationDrawing.Semantic>>() with
                    override x.Get(r) = r.semanticsList
                    override x.Set(r,v) = { r with semanticsList = v }
                    override x.Update(r,f) = { r with semanticsList = f r.semanticsList }
                }
            let selectedSemantic =
                { new Lens<CorrelationDrawing.SemanticApp, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.selectedSemantic
                    override x.Set(r,v) = { r with selectedSemantic = v }
                    override x.Update(r,f) = { r with selectedSemantic = f r.selectedSemantic }
                }
            let sortBy =
                { new Lens<CorrelationDrawing.SemanticApp, CorrelationDrawing.SemanticsSortingOption>() with
                    override x.Get(r) = r.sortBy
                    override x.Set(r,v) = { r with sortBy = v }
                    override x.Update(r,f) = { r with sortBy = f r.sortBy }
                }
            let creatingNew =
                { new Lens<CorrelationDrawing.SemanticApp, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.creatingNew
                    override x.Set(r,v) = { r with creatingNew = v }
                    override x.Update(r,f) = { r with creatingNew = f r.creatingNew }
                }
    
    
    type MAnnotationPoint(__initial : CorrelationDrawing.AnnotationPoint) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.AnnotationPoint> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.AnnotationPoint>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.AnnotationPoint>
        let _point = ResetMod.Create(__initial.point)
        let _selected = ResetMod.Create(__initial.selected)
        
        member x.point = _point :> IMod<_>
        member x.selected = _selected :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.AnnotationPoint) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_point,v.point)
                ResetMod.Update(_selected,v.selected)
                
        
        static member Create(__initial : CorrelationDrawing.AnnotationPoint) : MAnnotationPoint = MAnnotationPoint(__initial)
        static member Update(m : MAnnotationPoint, v : CorrelationDrawing.AnnotationPoint) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.AnnotationPoint> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AnnotationPoint =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let point =
                { new Lens<CorrelationDrawing.AnnotationPoint, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.point
                    override x.Set(r,v) = { r with point = v }
                    override x.Update(r,f) = { r with point = f r.point }
                }
            let selected =
                { new Lens<CorrelationDrawing.AnnotationPoint, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.selected
                    override x.Set(r,v) = { r with selected = v }
                    override x.Update(r,f) = { r with selected = f r.selected }
                }
    
    
    type MAnnotation(__initial : CorrelationDrawing.Annotation) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Annotation> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.Annotation>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Annotation>
        let _selected = ResetMod.Create(__initial.selected)
        let _semanticId = ResetMod.Create(__initial.semanticId)
        let _points = MList.Create(__initial.points, (fun v -> MAnnotationPoint.Create(v)), (fun (m,v) -> MAnnotationPoint.Update(m, v)), (fun v -> v))
        let _segments = MList.Create(__initial.segments, (fun v -> MList.Create(v)), (fun (m,v) -> MList.Update(m, v)), (fun v -> v :> alist<_>))
        let _visible = ResetMod.Create(__initial.visible)
        let _text = ResetMod.Create(__initial.text)
        let _overrideStyle = MOption.Create(__initial.overrideStyle, (fun v -> MStyle.Create(v)), (fun (m,v) -> MStyle.Update(m, v)), (fun v -> v))
        let _overrideLevel = MOption.Create(__initial.overrideLevel)
        let _grainSize = ResetMod.Create(__initial.grainSize)
        
        member x.id = __current.Value.id
        member x.geometry = __current.Value.geometry
        member x.projection = __current.Value.projection
        member x.semanticType = __current.Value.semanticType
        member x.selected = _selected :> IMod<_>
        member x.semanticId = _semanticId :> IMod<_>
        member x.points = _points :> alist<_>
        member x.segments = _segments :> alist<_>
        member x.visible = _visible :> IMod<_>
        member x.text = _text :> IMod<_>
        member x.overrideStyle = _overrideStyle :> IMod<_>
        member x.overrideLevel = _overrideLevel :> IMod<_>
        member x.grainSize = _grainSize :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.Annotation) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_selected,v.selected)
                ResetMod.Update(_semanticId,v.semanticId)
                MList.Update(_points, v.points)
                MList.Update(_segments, v.segments)
                ResetMod.Update(_visible,v.visible)
                ResetMod.Update(_text,v.text)
                MOption.Update(_overrideStyle, v.overrideStyle)
                MOption.Update(_overrideLevel, v.overrideLevel)
                ResetMod.Update(_grainSize,v.grainSize)
                
        
        static member Create(__initial : CorrelationDrawing.Annotation) : MAnnotation = MAnnotation(__initial)
        static member Update(m : MAnnotation, v : CorrelationDrawing.Annotation) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.Annotation> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Annotation =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<CorrelationDrawing.Annotation, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let geometry =
                { new Lens<CorrelationDrawing.Annotation, CorrelationDrawing.GeometryType>() with
                    override x.Get(r) = r.geometry
                    override x.Set(r,v) = { r with geometry = v }
                    override x.Update(r,f) = { r with geometry = f r.geometry }
                }
            let projection =
                { new Lens<CorrelationDrawing.Annotation, CorrelationDrawing.Projection>() with
                    override x.Get(r) = r.projection
                    override x.Set(r,v) = { r with projection = v }
                    override x.Update(r,f) = { r with projection = f r.projection }
                }
            let semanticType =
                { new Lens<CorrelationDrawing.Annotation, CorrelationDrawing.SemanticType>() with
                    override x.Get(r) = r.semanticType
                    override x.Set(r,v) = { r with semanticType = v }
                    override x.Update(r,f) = { r with semanticType = f r.semanticType }
                }
            let selected =
                { new Lens<CorrelationDrawing.Annotation, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.selected
                    override x.Set(r,v) = { r with selected = v }
                    override x.Update(r,f) = { r with selected = f r.selected }
                }
            let semanticId =
                { new Lens<CorrelationDrawing.Annotation, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.semanticId
                    override x.Set(r,v) = { r with semanticId = v }
                    override x.Update(r,f) = { r with semanticId = f r.semanticId }
                }
            let points =
                { new Lens<CorrelationDrawing.Annotation, Aardvark.Base.plist<CorrelationDrawing.AnnotationPoint>>() with
                    override x.Get(r) = r.points
                    override x.Set(r,v) = { r with points = v }
                    override x.Update(r,f) = { r with points = f r.points }
                }
            let segments =
                { new Lens<CorrelationDrawing.Annotation, Aardvark.Base.plist<Aardvark.Base.plist<Aardvark.Base.V3d>>>() with
                    override x.Get(r) = r.segments
                    override x.Set(r,v) = { r with segments = v }
                    override x.Update(r,f) = { r with segments = f r.segments }
                }
            let visible =
                { new Lens<CorrelationDrawing.Annotation, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.visible
                    override x.Set(r,v) = { r with visible = v }
                    override x.Update(r,f) = { r with visible = f r.visible }
                }
            let text =
                { new Lens<CorrelationDrawing.Annotation, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.text
                    override x.Set(r,v) = { r with text = v }
                    override x.Update(r,f) = { r with text = f r.text }
                }
            let overrideStyle =
                { new Lens<CorrelationDrawing.Annotation, Microsoft.FSharp.Core.option<CorrelationDrawing.Style>>() with
                    override x.Get(r) = r.overrideStyle
                    override x.Set(r,v) = { r with overrideStyle = v }
                    override x.Update(r,f) = { r with overrideStyle = f r.overrideStyle }
                }
            let overrideLevel =
                { new Lens<CorrelationDrawing.Annotation, Microsoft.FSharp.Core.option<Microsoft.FSharp.Core.int>>() with
                    override x.Get(r) = r.overrideLevel
                    override x.Set(r,v) = { r with overrideLevel = v }
                    override x.Update(r,f) = { r with overrideLevel = f r.overrideLevel }
                }
            let grainSize =
                { new Lens<CorrelationDrawing.Annotation, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.grainSize
                    override x.Set(r,v) = { r with grainSize = v }
                    override x.Update(r,f) = { r with grainSize = f r.grainSize }
                }
    
    
    type MHorizon(__initial : CorrelationDrawing.Horizon) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Horizon> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.Horizon>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Horizon>
        let _annotation = MAnnotation.Create(__initial.annotation)
        let _children = MList.Create(__initial.children, (fun v -> MAnnotation.Create(v)), (fun (m,v) -> MAnnotation.Update(m, v)), (fun v -> v))
        
        member x.id = __current.Value.id
        member x.annotation = _annotation
        member x.children = _children :> alist<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.Horizon) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MAnnotation.Update(_annotation, v.annotation)
                MList.Update(_children, v.children)
                
        
        static member Create(__initial : CorrelationDrawing.Horizon) : MHorizon = MHorizon(__initial)
        static member Update(m : MHorizon, v : CorrelationDrawing.Horizon) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.Horizon> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Horizon =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<CorrelationDrawing.Horizon, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let annotation =
                { new Lens<CorrelationDrawing.Horizon, CorrelationDrawing.Annotation>() with
                    override x.Get(r) = r.annotation
                    override x.Set(r,v) = { r with annotation = v }
                    override x.Update(r,f) = { r with annotation = f r.annotation }
                }
            let children =
                { new Lens<CorrelationDrawing.Horizon, Aardvark.Base.plist<CorrelationDrawing.Annotation>>() with
                    override x.Get(r) = r.children
                    override x.Set(r,v) = { r with children = v }
                    override x.Update(r,f) = { r with children = f r.children }
                }
    
    
    type MBorder(__initial : CorrelationDrawing.Border) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Border> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.Border>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Border>
        let _anno = MAnnotation.Create(__initial.anno)
        let _point = ResetMod.Create(__initial.point)
        
        member x.anno = _anno
        member x.point = _point :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.Border) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MAnnotation.Update(_anno, v.anno)
                ResetMod.Update(_point,v.point)
                
        
        static member Create(__initial : CorrelationDrawing.Border) : MBorder = MBorder(__initial)
        static member Update(m : MBorder, v : CorrelationDrawing.Border) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.Border> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Border =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let anno =
                { new Lens<CorrelationDrawing.Border, CorrelationDrawing.Annotation>() with
                    override x.Get(r) = r.anno
                    override x.Set(r,v) = { r with anno = v }
                    override x.Update(r,f) = { r with anno = f r.anno }
                }
            let point =
                { new Lens<CorrelationDrawing.Border, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.point
                    override x.Set(r,v) = { r with point = v }
                    override x.Update(r,f) = { r with point = f r.point }
                }
    
    
    type MLogNode(__initial : CorrelationDrawing.LogNode) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.LogNode> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.LogNode>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.LogNode>
        let _label = ResetMod.Create(__initial.label)
        let _lBoundary = MBorder.Create(__initial.lBoundary)
        let _uBoundary = MBorder.Create(__initial.uBoundary)
        let _children = MList.Create(__initial.children, (fun v -> MLogNode.Create(v)), (fun (m,v) -> MLogNode.Update(m, v)), (fun v -> v))
        let _elevation = ResetMod.Create(__initial.elevation)
        let _range = ResetMod.Create(__initial.range)
        let _logYPos = ResetMod.Create(__initial.logYPos)
        let _pos = ResetMod.Create(__initial.pos)
        let _size = ResetMod.Create(__initial.size)
        
        member x.label = _label :> IMod<_>
        member x.lBoundary = _lBoundary
        member x.uBoundary = _uBoundary
        member x.children = _children :> alist<_>
        member x.elevation = _elevation :> IMod<_>
        member x.range = _range :> IMod<_>
        member x.logYPos = _logYPos :> IMod<_>
        member x.pos = _pos :> IMod<_>
        member x.size = _size :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.LogNode) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_label,v.label)
                MBorder.Update(_lBoundary, v.lBoundary)
                MBorder.Update(_uBoundary, v.uBoundary)
                MList.Update(_children, v.children)
                ResetMod.Update(_elevation,v.elevation)
                ResetMod.Update(_range,v.range)
                ResetMod.Update(_logYPos,v.logYPos)
                ResetMod.Update(_pos,v.pos)
                ResetMod.Update(_size,v.size)
                
        
        static member Create(__initial : CorrelationDrawing.LogNode) : MLogNode = MLogNode(__initial)
        static member Update(m : MLogNode, v : CorrelationDrawing.LogNode) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.LogNode> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module LogNode =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let label =
                { new Lens<CorrelationDrawing.LogNode, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.label
                    override x.Set(r,v) = { r with label = v }
                    override x.Update(r,f) = { r with label = f r.label }
                }
            let lBoundary =
                { new Lens<CorrelationDrawing.LogNode, CorrelationDrawing.Border>() with
                    override x.Get(r) = r.lBoundary
                    override x.Set(r,v) = { r with lBoundary = v }
                    override x.Update(r,f) = { r with lBoundary = f r.lBoundary }
                }
            let uBoundary =
                { new Lens<CorrelationDrawing.LogNode, CorrelationDrawing.Border>() with
                    override x.Get(r) = r.uBoundary
                    override x.Set(r,v) = { r with uBoundary = v }
                    override x.Update(r,f) = { r with uBoundary = f r.uBoundary }
                }
            let children =
                { new Lens<CorrelationDrawing.LogNode, Aardvark.Base.plist<CorrelationDrawing.LogNode>>() with
                    override x.Get(r) = r.children
                    override x.Set(r,v) = { r with children = v }
                    override x.Update(r,f) = { r with children = f r.children }
                }
            let elevation =
                { new Lens<CorrelationDrawing.LogNode, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.elevation
                    override x.Set(r,v) = { r with elevation = v }
                    override x.Update(r,f) = { r with elevation = f r.elevation }
                }
            let range =
                { new Lens<CorrelationDrawing.LogNode, CorrelationDrawing.Rangef>() with
                    override x.Get(r) = r.range
                    override x.Set(r,v) = { r with range = v }
                    override x.Update(r,f) = { r with range = f r.range }
                }
            let logYPos =
                { new Lens<CorrelationDrawing.LogNode, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.logYPos
                    override x.Set(r,v) = { r with logYPos = v }
                    override x.Update(r,f) = { r with logYPos = f r.logYPos }
                }
            let pos =
                { new Lens<CorrelationDrawing.LogNode, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.pos
                    override x.Set(r,v) = { r with pos = v }
                    override x.Update(r,f) = { r with pos = f r.pos }
                }
            let size =
                { new Lens<CorrelationDrawing.LogNode, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.size
                    override x.Set(r,v) = { r with size = v }
                    override x.Update(r,f) = { r with size = f r.size }
                }
    
    
    type MGeologicalLog(__initial : CorrelationDrawing.GeologicalLog) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.GeologicalLog> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.GeologicalLog>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.GeologicalLog>
        let _annoPoints = ResetMod.Create(__initial.annoPoints)
        let _nodes = MList.Create(__initial.nodes, (fun v -> MLogNode.Create(v)), (fun (m,v) -> MLogNode.Update(m, v)), (fun v -> v))
        let _range = ResetMod.Create(__initial.range)
        let _camera = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.camera)
        
        member x.id = __current.Value.id
        member x.annoPoints = _annoPoints :> IMod<_>
        member x.nodes = _nodes :> alist<_>
        member x.range = _range :> IMod<_>
        member x.camera = _camera
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.GeologicalLog) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_annoPoints,v.annoPoints)
                MList.Update(_nodes, v.nodes)
                ResetMod.Update(_range,v.range)
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_camera, v.camera)
                
        
        static member Create(__initial : CorrelationDrawing.GeologicalLog) : MGeologicalLog = MGeologicalLog(__initial)
        static member Update(m : MGeologicalLog, v : CorrelationDrawing.GeologicalLog) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.GeologicalLog> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module GeologicalLog =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<CorrelationDrawing.GeologicalLog, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let annoPoints =
                { new Lens<CorrelationDrawing.GeologicalLog, Microsoft.FSharp.Collections.list<(Aardvark.Base.V3d * CorrelationDrawing.Annotation)>>() with
                    override x.Get(r) = r.annoPoints
                    override x.Set(r,v) = { r with annoPoints = v }
                    override x.Update(r,f) = { r with annoPoints = f r.annoPoints }
                }
            let nodes =
                { new Lens<CorrelationDrawing.GeologicalLog, Aardvark.Base.plist<CorrelationDrawing.LogNode>>() with
                    override x.Get(r) = r.nodes
                    override x.Set(r,v) = { r with nodes = v }
                    override x.Update(r,f) = { r with nodes = f r.nodes }
                }
            let range =
                { new Lens<CorrelationDrawing.GeologicalLog, CorrelationDrawing.Rangef>() with
                    override x.Get(r) = r.range
                    override x.Set(r,v) = { r with range = v }
                    override x.Update(r,f) = { r with range = f r.range }
                }
            let camera =
                { new Lens<CorrelationDrawing.GeologicalLog, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.camera
                    override x.Set(r,v) = { r with camera = v }
                    override x.Update(r,f) = { r with camera = f r.camera }
                }
    
    
    type MCorrelationPlotApp(__initial : CorrelationDrawing.CorrelationPlotApp) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationPlotApp> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.CorrelationPlotApp>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationPlotApp>
        let _logs = MList.Create(__initial.logs, (fun v -> MGeologicalLog.Create(v)), (fun (m,v) -> MGeologicalLog.Update(m, v)), (fun v -> v))
        let _working = ResetMod.Create(__initial.working)
        let _selectedLog = MOption.Create(__initial.selectedLog)
        let _creatingNew = ResetMod.Create(__initial.creatingNew)
        
        member x.logs = _logs :> alist<_>
        member x.working = _working :> IMod<_>
        member x.selectedLog = _selectedLog :> IMod<_>
        member x.creatingNew = _creatingNew :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.CorrelationPlotApp) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_logs, v.logs)
                ResetMod.Update(_working,v.working)
                MOption.Update(_selectedLog, v.selectedLog)
                ResetMod.Update(_creatingNew,v.creatingNew)
                
        
        static member Create(__initial : CorrelationDrawing.CorrelationPlotApp) : MCorrelationPlotApp = MCorrelationPlotApp(__initial)
        static member Update(m : MCorrelationPlotApp, v : CorrelationDrawing.CorrelationPlotApp) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.CorrelationPlotApp> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CorrelationPlotApp =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let logs =
                { new Lens<CorrelationDrawing.CorrelationPlotApp, Aardvark.Base.plist<CorrelationDrawing.GeologicalLog>>() with
                    override x.Get(r) = r.logs
                    override x.Set(r,v) = { r with logs = v }
                    override x.Update(r,f) = { r with logs = f r.logs }
                }
            let working =
                { new Lens<CorrelationDrawing.CorrelationPlotApp, Microsoft.FSharp.Collections.list<(Aardvark.Base.V3d * CorrelationDrawing.Annotation)>>() with
                    override x.Get(r) = r.working
                    override x.Set(r,v) = { r with working = v }
                    override x.Update(r,f) = { r with working = f r.working }
                }
            let selectedLog =
                { new Lens<CorrelationDrawing.CorrelationPlotApp, Microsoft.FSharp.Core.option<Microsoft.FSharp.Core.string>>() with
                    override x.Get(r) = r.selectedLog
                    override x.Set(r,v) = { r with selectedLog = v }
                    override x.Update(r,f) = { r with selectedLog = f r.selectedLog }
                }
            let creatingNew =
                { new Lens<CorrelationDrawing.CorrelationPlotApp, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.creatingNew
                    override x.Set(r,v) = { r with creatingNew = v }
                    override x.Update(r,f) = { r with creatingNew = f r.creatingNew }
                }
    
    
    type MCorrelationDrawingModel(__initial : CorrelationDrawing.CorrelationDrawingModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationDrawingModel> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.CorrelationDrawingModel>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationDrawingModel>
        let _draw = ResetMod.Create(__initial.draw)
        let _hoverPosition = MOption.Create(__initial.hoverPosition)
        let _working = MOption.Create(__initial.working, (fun v -> MAnnotation.Create(v)), (fun (m,v) -> MAnnotation.Update(m, v)), (fun v -> v))
        let _projection = ResetMod.Create(__initial.projection)
        let _geometry = ResetMod.Create(__initial.geometry)
        let _selectedAnnotation = MOption.Create(__initial.selectedAnnotation)
        let _annotations = MList.Create(__initial.annotations, (fun v -> MAnnotation.Create(v)), (fun (m,v) -> MAnnotation.Update(m, v)), (fun v -> v))
        let _exportPath = ResetMod.Create(__initial.exportPath)
        
        member x.draw = _draw :> IMod<_>
        member x.hoverPosition = _hoverPosition :> IMod<_>
        member x.working = _working :> IMod<_>
        member x.projection = _projection :> IMod<_>
        member x.geometry = _geometry :> IMod<_>
        member x.selectedAnnotation = _selectedAnnotation :> IMod<_>
        member x.annotations = _annotations :> alist<_>
        member x.exportPath = _exportPath :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.CorrelationDrawingModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_draw,v.draw)
                MOption.Update(_hoverPosition, v.hoverPosition)
                MOption.Update(_working, v.working)
                ResetMod.Update(_projection,v.projection)
                ResetMod.Update(_geometry,v.geometry)
                MOption.Update(_selectedAnnotation, v.selectedAnnotation)
                MList.Update(_annotations, v.annotations)
                ResetMod.Update(_exportPath,v.exportPath)
                
        
        static member Create(__initial : CorrelationDrawing.CorrelationDrawingModel) : MCorrelationDrawingModel = MCorrelationDrawingModel(__initial)
        static member Update(m : MCorrelationDrawingModel, v : CorrelationDrawing.CorrelationDrawingModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.CorrelationDrawingModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CorrelationDrawingModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let draw =
                { new Lens<CorrelationDrawing.CorrelationDrawingModel, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.draw
                    override x.Set(r,v) = { r with draw = v }
                    override x.Update(r,f) = { r with draw = f r.draw }
                }
            let hoverPosition =
                { new Lens<CorrelationDrawing.CorrelationDrawingModel, Microsoft.FSharp.Core.option<Aardvark.Base.Trafo3d>>() with
                    override x.Get(r) = r.hoverPosition
                    override x.Set(r,v) = { r with hoverPosition = v }
                    override x.Update(r,f) = { r with hoverPosition = f r.hoverPosition }
                }
            let working =
                { new Lens<CorrelationDrawing.CorrelationDrawingModel, Microsoft.FSharp.Core.option<CorrelationDrawing.Annotation>>() with
                    override x.Get(r) = r.working
                    override x.Set(r,v) = { r with working = v }
                    override x.Update(r,f) = { r with working = f r.working }
                }
            let projection =
                { new Lens<CorrelationDrawing.CorrelationDrawingModel, CorrelationDrawing.Projection>() with
                    override x.Get(r) = r.projection
                    override x.Set(r,v) = { r with projection = v }
                    override x.Update(r,f) = { r with projection = f r.projection }
                }
            let geometry =
                { new Lens<CorrelationDrawing.CorrelationDrawingModel, CorrelationDrawing.GeometryType>() with
                    override x.Get(r) = r.geometry
                    override x.Set(r,v) = { r with geometry = v }
                    override x.Update(r,f) = { r with geometry = f r.geometry }
                }
            let selectedAnnotation =
                { new Lens<CorrelationDrawing.CorrelationDrawingModel, Microsoft.FSharp.Core.option<Microsoft.FSharp.Core.string>>() with
                    override x.Get(r) = r.selectedAnnotation
                    override x.Set(r,v) = { r with selectedAnnotation = v }
                    override x.Update(r,f) = { r with selectedAnnotation = f r.selectedAnnotation }
                }
            let annotations =
                { new Lens<CorrelationDrawing.CorrelationDrawingModel, Aardvark.Base.plist<CorrelationDrawing.Annotation>>() with
                    override x.Get(r) = r.annotations
                    override x.Set(r,v) = { r with annotations = v }
                    override x.Update(r,f) = { r with annotations = f r.annotations }
                }
            let exportPath =
                { new Lens<CorrelationDrawing.CorrelationDrawingModel, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.exportPath
                    override x.Set(r,v) = { r with exportPath = v }
                    override x.Update(r,f) = { r with exportPath = f r.exportPath }
                }
    
    
    type MCorrelationAppModel(__initial : CorrelationDrawing.CorrelationAppModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationAppModel> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.CorrelationAppModel>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationAppModel>
        let _camera = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.camera)
        let _rendering = MRenderingParameters.Create(__initial.rendering)
        let _drawing = MCorrelationDrawingModel.Create(__initial.drawing)
        
        member x.camera = _camera
        member x.rendering = _rendering
        member x.drawing = _drawing
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.CorrelationAppModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_camera, v.camera)
                MRenderingParameters.Update(_rendering, v.rendering)
                MCorrelationDrawingModel.Update(_drawing, v.drawing)
                
        
        static member Create(__initial : CorrelationDrawing.CorrelationAppModel) : MCorrelationAppModel = MCorrelationAppModel(__initial)
        static member Update(m : MCorrelationAppModel, v : CorrelationDrawing.CorrelationAppModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.CorrelationAppModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CorrelationAppModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let camera =
                { new Lens<CorrelationDrawing.CorrelationAppModel, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.camera
                    override x.Set(r,v) = { r with camera = v }
                    override x.Update(r,f) = { r with camera = f r.camera }
                }
            let rendering =
                { new Lens<CorrelationDrawing.CorrelationAppModel, CorrelationDrawing.RenderingParameters>() with
                    override x.Get(r) = r.rendering
                    override x.Set(r,v) = { r with rendering = v }
                    override x.Update(r,f) = { r with rendering = f r.rendering }
                }
            let drawing =
                { new Lens<CorrelationDrawing.CorrelationAppModel, CorrelationDrawing.CorrelationDrawingModel>() with
                    override x.Get(r) = r.drawing
                    override x.Set(r,v) = { r with drawing = v }
                    override x.Update(r,f) = { r with drawing = f r.drawing }
                }
    
    
    type MPages(__initial : CorrelationDrawing.Pages) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Pages> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.Pages>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Pages>
        let _cameraState = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.cameraState)
        let _cullMode = ResetMod.Create(__initial.cullMode)
        let _fill = ResetMod.Create(__initial.fill)
        let _dockConfig = ResetMod.Create(__initial.dockConfig)
        let _drawingApp = MCorrelationAppModel.Create(__initial.drawingApp)
        let _semanticApp = MSemanticApp.Create(__initial.semanticApp)
        let _corrPlotApp = MCorrelationPlotApp.Create(__initial.corrPlotApp)
        
        member x.past = __current.Value.past
        member x.future = __current.Value.future
        member x.cameraState = _cameraState
        member x.cullMode = _cullMode :> IMod<_>
        member x.fill = _fill :> IMod<_>
        member x.dockConfig = _dockConfig :> IMod<_>
        member x.drawingApp = _drawingApp
        member x.semanticApp = _semanticApp
        member x.corrPlotApp = _corrPlotApp
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.Pages) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_cameraState, v.cameraState)
                ResetMod.Update(_cullMode,v.cullMode)
                ResetMod.Update(_fill,v.fill)
                ResetMod.Update(_dockConfig,v.dockConfig)
                MCorrelationAppModel.Update(_drawingApp, v.drawingApp)
                MSemanticApp.Update(_semanticApp, v.semanticApp)
                MCorrelationPlotApp.Update(_corrPlotApp, v.corrPlotApp)
                
        
        static member Create(__initial : CorrelationDrawing.Pages) : MPages = MPages(__initial)
        static member Update(m : MPages, v : CorrelationDrawing.Pages) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.Pages> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Pages =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let past =
                { new Lens<CorrelationDrawing.Pages, Microsoft.FSharp.Core.Option<CorrelationDrawing.Pages>>() with
                    override x.Get(r) = r.past
                    override x.Set(r,v) = { r with past = v }
                    override x.Update(r,f) = { r with past = f r.past }
                }
            let future =
                { new Lens<CorrelationDrawing.Pages, Microsoft.FSharp.Core.Option<CorrelationDrawing.Pages>>() with
                    override x.Get(r) = r.future
                    override x.Set(r,v) = { r with future = v }
                    override x.Update(r,f) = { r with future = f r.future }
                }
            let cameraState =
                { new Lens<CorrelationDrawing.Pages, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.cameraState
                    override x.Set(r,v) = { r with cameraState = v }
                    override x.Update(r,f) = { r with cameraState = f r.cameraState }
                }
            let cullMode =
                { new Lens<CorrelationDrawing.Pages, Aardvark.Base.Rendering.CullMode>() with
                    override x.Get(r) = r.cullMode
                    override x.Set(r,v) = { r with cullMode = v }
                    override x.Update(r,f) = { r with cullMode = f r.cullMode }
                }
            let fill =
                { new Lens<CorrelationDrawing.Pages, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.fill
                    override x.Set(r,v) = { r with fill = v }
                    override x.Update(r,f) = { r with fill = f r.fill }
                }
            let dockConfig =
                { new Lens<CorrelationDrawing.Pages, Aardvark.UI.Primitives.DockConfig>() with
                    override x.Get(r) = r.dockConfig
                    override x.Set(r,v) = { r with dockConfig = v }
                    override x.Update(r,f) = { r with dockConfig = f r.dockConfig }
                }
            let drawingApp =
                { new Lens<CorrelationDrawing.Pages, CorrelationDrawing.CorrelationAppModel>() with
                    override x.Get(r) = r.drawingApp
                    override x.Set(r,v) = { r with drawingApp = v }
                    override x.Update(r,f) = { r with drawingApp = f r.drawingApp }
                }
            let semanticApp =
                { new Lens<CorrelationDrawing.Pages, CorrelationDrawing.SemanticApp>() with
                    override x.Get(r) = r.semanticApp
                    override x.Set(r,v) = { r with semanticApp = v }
                    override x.Update(r,f) = { r with semanticApp = f r.semanticApp }
                }
            let corrPlotApp =
                { new Lens<CorrelationDrawing.Pages, CorrelationDrawing.CorrelationPlotApp>() with
                    override x.Get(r) = r.corrPlotApp
                    override x.Set(r,v) = { r with corrPlotApp = v }
                    override x.Update(r,f) = { r with corrPlotApp = f r.corrPlotApp }
                }
