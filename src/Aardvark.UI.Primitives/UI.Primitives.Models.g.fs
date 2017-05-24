namespace Aardvark.UI

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

[<AutoOpen>]
module Mutable =

    [<StructuredFormatDisplay("{AsString}")>]
    [<System.Runtime.CompilerServices.Extension>]
    type MNumericInput private(__initial : Aardvark.UI.NumericInput) =
        let mutable __current = __initial
        let _value = ResetMod(__initial.value)
        let _min = ResetMod(__initial.min)
        let _max = ResetMod(__initial.max)
        let _step = ResetMod(__initial.step)
        let _format = ResetMod(__initial.format)
        
        member x.value = _value :> IMod<_>
        member x.min = _min :> IMod<_>
        member x.max = _max :> IMod<_>
        member x.step = _step :> IMod<_>
        member x.format = _format :> IMod<_>
        
        member x.Update(__model : Aardvark.UI.NumericInput) =
            if not (Object.ReferenceEquals(__model, __current)) then
                __current <- __model
                _value.Update(__model.value)
                _min.Update(__model.min)
                _max.Update(__model.max)
                _step.Update(__model.step)
                _format.Update(__model.format)
        
        static member Update(__self : MNumericInput, __model : Aardvark.UI.NumericInput) = __self.Update(__model)
        
        static member Create(initial) = MNumericInput(initial)
        
        override x.ToString() = __current.ToString()
        member private x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MNumericInput =
        let inline value (m : MNumericInput) = m.value
        let inline min (m : MNumericInput) = m.min
        let inline max (m : MNumericInput) = m.max
        let inline step (m : MNumericInput) = m.step
        let inline format (m : MNumericInput) = m.format
    
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module NumericInput =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let value =
                { new Lens<Aardvark.UI.NumericInput, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.value
                    override x.Set(r,v) = { r with value = v }
                    override x.Update(r,f) = { r with value = f r.value }
                }
            let min =
                { new Lens<Aardvark.UI.NumericInput, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.min
                    override x.Set(r,v) = { r with min = v }
                    override x.Update(r,f) = { r with min = f r.min }
                }
            let max =
                { new Lens<Aardvark.UI.NumericInput, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.max
                    override x.Set(r,v) = { r with max = v }
                    override x.Update(r,f) = { r with max = f r.max }
                }
            let step =
                { new Lens<Aardvark.UI.NumericInput, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.step
                    override x.Set(r,v) = { r with step = v }
                    override x.Update(r,f) = { r with step = f r.step }
                }
            let format =
                { new Lens<Aardvark.UI.NumericInput, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.format
                    override x.Set(r,v) = { r with format = v }
                    override x.Update(r,f) = { r with format = f r.format }
                }
    [<AbstractClass; System.Runtime.CompilerServices.Extension; StructuredFormatDisplay("{AsString}")>]
    type MLeafValue() =
        abstract member TryUpdate : Aardvark.UI.LeafValue -> bool
        abstract member AsString : string
        
        static member private CreateValue(__model : Aardvark.UI.LeafValue) = 
            match __model with
                | Number(item) -> MNumber(__model, item) :> MLeafValue
                | Text(item) -> MText(__model, item) :> MLeafValue
        
        static member Create(v : Aardvark.UI.LeafValue) =
            ResetMod(MLeafValue.CreateValue v) :> IMod<_>
        
        [<System.Runtime.CompilerServices.Extension>]
        static member Update(m : IMod<MLeafValue>, v : Aardvark.UI.LeafValue) =
            let m = unbox<ResetMod<MLeafValue>> m
            if not (m.GetValue().TryUpdate v) then
                m.Update(MLeafValue.CreateValue v)
    
    and private MNumber(__initial : Aardvark.UI.LeafValue, item : Microsoft.FSharp.Core.int) =
        inherit MLeafValue()
        
        let mutable __current = __initial
        let _item = ResetMod(item)
        member x.item = _item :> IMod<_>
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Aardvark.UI.LeafValue) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | Number(item) -> 
                        __current <- __model
                        _item.Update(item)
                        true
                    | _ -> false
    
    and private MText(__initial : Aardvark.UI.LeafValue, item : Microsoft.FSharp.Core.string) =
        inherit MLeafValue()
        
        let mutable __current = __initial
        let _item = ResetMod(item)
        member x.item = _item :> IMod<_>
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Aardvark.UI.LeafValue) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | Text(item) -> 
                        __current <- __model
                        _item.Update(item)
                        true
                    | _ -> false
    
    
    [<AutoOpen>]
    module MLeafValuePatterns =
        let (|MNumber|MText|) (m : MLeafValue) =
            match m with
            | :? MNumber as v -> MNumber(v.item)
            | :? MText as v -> MText(v.item)
            | _ -> failwith "impossible"
    
    
    
    
    [<StructuredFormatDisplay("{AsString}")>]
    [<System.Runtime.CompilerServices.Extension>]
    type MProperties private(__initial : Aardvark.UI.Properties) =
        let mutable __current = __initial
        let _isExpanded = ResetMod(__initial.isExpanded)
        let _isSelected = ResetMod(__initial.isSelected)
        let _isActive = ResetMod(__initial.isActive)
        
        member x.isExpanded = _isExpanded :> IMod<_>
        member x.isSelected = _isSelected :> IMod<_>
        member x.isActive = _isActive :> IMod<_>
        
        member x.Update(__model : Aardvark.UI.Properties) =
            if not (Object.ReferenceEquals(__model, __current)) then
                __current <- __model
                _isExpanded.Update(__model.isExpanded)
                _isSelected.Update(__model.isSelected)
                _isActive.Update(__model.isActive)
        
        static member Update(__self : MProperties, __model : Aardvark.UI.Properties) = __self.Update(__model)
        
        static member Create(initial) = MProperties(initial)
        
        override x.ToString() = __current.ToString()
        member private x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MProperties =
        let inline isExpanded (m : MProperties) = m.isExpanded
        let inline isSelected (m : MProperties) = m.isSelected
        let inline isActive (m : MProperties) = m.isActive
    
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Properties =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let isExpanded =
                { new Lens<Aardvark.UI.Properties, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.isExpanded
                    override x.Set(r,v) = { r with isExpanded = v }
                    override x.Update(r,f) = { r with isExpanded = f r.isExpanded }
                }
            let isSelected =
                { new Lens<Aardvark.UI.Properties, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.isSelected
                    override x.Set(r,v) = { r with isSelected = v }
                    override x.Update(r,f) = { r with isSelected = f r.isSelected }
                }
            let isActive =
                { new Lens<Aardvark.UI.Properties, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.isActive
                    override x.Set(r,v) = { r with isActive = v }
                    override x.Update(r,f) = { r with isActive = f r.isActive }
                }
    [<AbstractClass; System.Runtime.CompilerServices.Extension; StructuredFormatDisplay("{AsString}")>]
    type MTree() =
        abstract member TryUpdate : Aardvark.UI.Tree -> bool
        abstract member AsString : string
        
        static member private CreateValue(__model : Aardvark.UI.Tree) = 
            match __model with
                | Node(value, properties, children) -> MNode(__model, value, properties, children) :> MTree
                | Leaf(value) -> MLeaf(__model, value) :> MTree
        
        static member Create(v : Aardvark.UI.Tree) =
            ResetMod(MTree.CreateValue v) :> IMod<_>
        
        [<System.Runtime.CompilerServices.Extension>]
        static member Update(m : IMod<MTree>, v : Aardvark.UI.Tree) =
            let m = unbox<ResetMod<MTree>> m
            if not (m.GetValue().TryUpdate v) then
                m.Update(MTree.CreateValue v)
    
    and private MNode(__initial : Aardvark.UI.Tree, value : Aardvark.UI.LeafValue, properties : Aardvark.UI.Properties, children : Aardvark.Base.plist<Aardvark.UI.Tree>) =
        inherit MTree()
        
        let mutable __current = __initial
        let _value = MLeafValue.Create(value)
        let _properties = MProperties.Create(properties)
        let _children = ResetMapList(children, (fun _ -> MTree.Create), MTree.Update)
        member x.value = _value
        member x.properties = _properties
        member x.children = _children :> alist<_>
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Aardvark.UI.Tree) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | Node(value,properties,children) -> 
                        __current <- __model
                        _value.Update(value)
                        _properties.Update(properties)
                        _children.Update(children)
                        true
                    | _ -> false
    
    and private MLeaf(__initial : Aardvark.UI.Tree, value : Aardvark.UI.LeafValue) =
        inherit MTree()
        
        let mutable __current = __initial
        let _value = MLeafValue.Create(value)
        member x.value = _value
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Aardvark.UI.Tree) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | Leaf(value) -> 
                        __current <- __model
                        _value.Update(value)
                        true
                    | _ -> false
    
    
    [<AutoOpen>]
    module MTreePatterns =
        let (|MNode|MLeaf|) (m : MTree) =
            match m with
            | :? MNode as v -> MNode(v.value,v.properties,v.children)
            | :? MLeaf as v -> MLeaf(v.value)
            | _ -> failwith "impossible"
    
    
    
    
    [<StructuredFormatDisplay("{AsString}")>]
    [<System.Runtime.CompilerServices.Extension>]
    type MTreeModel private(__initial : Aardvark.UI.TreeModel) =
        let mutable __current = __initial
        let _data = MTree.Create(__initial.data)
        
        member x.data = _data
        
        member x.Update(__model : Aardvark.UI.TreeModel) =
            if not (Object.ReferenceEquals(__model, __current)) then
                __current <- __model
                _data.Update(__model.data)
        
        static member Update(__self : MTreeModel, __model : Aardvark.UI.TreeModel) = __self.Update(__model)
        
        static member Create(initial) = MTreeModel(initial)
        
        override x.ToString() = __current.ToString()
        member private x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MTreeModel =
        let inline data (m : MTreeModel) = m.data
    
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TreeModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let data =
                { new Lens<Aardvark.UI.TreeModel, Aardvark.UI.Tree>() with
                    override x.Get(r) = r.data
                    override x.Set(r,v) = { r with data = v }
                    override x.Update(r,f) = { r with data = f r.data }
                }
