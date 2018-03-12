namespace CorrelationDrawing

//open System
//open Aardvark.Base.Incremental
//open Aardvark.Base
//open Aardvark.UI
//
//module CorrelationUtilities =



//    let dropDownList (values : alist<'a>)(selected : IMod<Option<'a>>) (change : Option<'a> -> 'msg) (f : 'a ->string)  =
//
//        let attributes (name : string) =
//            AttributeMap.ofListCond [
//                always (attribute "value" (name))
//                onlyWhen (
//                        selected 
//                            |> Mod.map (
//                                fun x -> 
//                                    match x with
//                                        | Some s -> name = f s
//                                        | None   -> name = "-None-"
//                            )) (attribute "selected" "selected")
//            ]
//
//        let ortisOnChange  = 
//            let cb (i : int) =
//                let currentState = values.Content |> Mod.force
//                change (PList.tryAt (i-1) currentState)
//                    
//            onEvent "onchange" ["event.target.selectedIndex"] (fun x -> x |> List.head |> Int32.Parse |> cb)
//
//        Incremental.select (AttributeMap.ofList [ortisOnChange; style "color:black"]) 
//            (
//                alist {
//                    yield Incremental.option (attributes "-None-") (AList.ofList [text "-None-"])
//                    yield! values |> AList.mapi(fun i x -> Incremental.option (attributes (f x)) (AList.ofList [text (f x)]))
//                }
//            )
//
//
//    let dropDownList' (values : alist<'a>)(selected : IMod<IMod<Option<'a>>>)
//         (change : Option<'a> -> 'msg) (f : 'a -> string)  =
//        let existsSelected =
//            adaptive {
//                let! msel = selected
//                let! s = msel
//
//                return match s with
//                        | Some s -> true
//                        | None -> false
//            }
//
//        let attributes (name : string) =
//                AttributeMap.ofListCond [
//                    always (attribute "value" (name))
//                    onlyWhen (existsSelected
////                                |> Mod.map (
////                                    fun x -> 
////                                        match x with
////                                            | Some s -> name = f s
////                                            | None   -> name = "-None-"
//                                ) (attribute "selected" "selected")
//                ]
//            
//
//        let ortisOnChange  = 
//            let cb (i : int) =
//                let currentState = values.Content |> Mod.force
//                change (PList.tryAt (i-1) currentState)
//                    
//            onEvent "onchange" ["event.target.selectedIndex"] (fun x -> x |> List.head |> Int32.Parse |> cb)
//
//        Incremental.select (AttributeMap.ofList [ortisOnChange; style "color:black"]) 
//            (
//                alist {
//                    yield Incremental.option (attributes "-None-") (AList.ofList [text "-None-"])
//                    // |> AList.mapi((f x) |> Mod.map(fun (y : IMod<'a>) -> fun i x -> Incremental.option (attributes y AList.ofList [text y])))
//                    // let domNode = values |> AList.mapi(fun i x -> Incremental.option (attributes (f x)) (AList.ofList [text (f x)]))
//                    let domNode = 
//                        values |> AList.mapi(fun i x -> Incremental.option (attributes (f x)) (AList.ofList [text (f x)]))
//                           // |> AList.mapi(fun _ x -> Incremental.option  (Mod.map (fun y ->  (attributes y)) (f x)) ( (AList.ofList [Mod.map (fun y -> (text y)) (f x)] ) ))
//                    yield! domNode
//                }
//            )

//    let dropDownList'' (values : alist<'a>)(selected : IMod<IMod<Option<'a>>>)
//         (change : Option<'a> -> 'msg) (f : 'a -> IMod<string>)  =
//        let existsSelected =
//            adaptive {
//                let! msel = selected
//                let! s = msel
//
//                return match s with
//                        | Some s -> true
//                        | None -> false
//            }
//
//        let attributes (name : string) =
//            let notSelected = (attribute "value" (name))
//            let selected = (attribute "selected" "selected")
//            let attrMap = 
//                AttributeMap.ofListCond [
//                    always (notSelected)
//                    onlyWhen (existsSelected) (selected)
//                ]
//            let debug = Mod.force attrMap.Content
//            attrMap
////                AttributeMap.ofListCond [
////                    always (attribute "value" (name))
////                    onlyWhen (existsSelected
//////                                |> Mod.map (
//////                                    fun x -> 
//////                                        match x with
//////                                            | Some s -> name = f s
//////                                            | None   -> name = "-None-"
////                                ) (attribute "selected" "selected")
////                ]
//            
//
//        let ortisOnChange  = 
//            let cb (i : int) =
//                let currentState = values.Content |> Mod.force
//                let selectedElem = PList.tryAt (i)
//                //change (PList.tryAt (i-1) currentState)
//                change (selectedElem currentState)
//                    
//            onEvent "onchange" ["event.target.selectedIndex"] 
//                (fun x -> 
//                    x 
//                        |> List.head 
//                        |> Int32.Parse 
//                        |> cb)
//        
//
//        Incremental.select (AttributeMap.ofList [ortisOnChange; style "color:black"]) 
//            (
//                alist {
//                    let domNode = 
//                        values |> AList.mapi(fun i x -> Incremental.option (attributes (Mod.force (f x))) (AList.ofList [Incremental.text (f x)]))
//                    yield! domNode
//                }
//            )


//    let dropDownListR (values : alist<'a>)(selected : IMod<IMod<Option<'a>>>)
//         (change : Option<'a> -> 'msg) (getDisplayString : 'a -> IMod<string>) 
//         (getIsSelected : 'a -> IMod<bool>) =
//
//        let attributes (value : 'a) (name : string) =
//            let notSelected = (attribute "value" (name))
//            let selAttr = (attribute "selected" "selected")
//            let attrMap = 
//                AttributeMap.ofListCond [
//                    always (notSelected)
//                    onlyWhen (getIsSelected value) (selAttr)
//                ]
//            let debug = Mod.force attrMap.Content
//            attrMap
//           
//
//        let rOnChange  = 
//            let cb (i : int) =
//                let currentState = values.Content |> Mod.force
//                let selectedElem = PList.tryAt (i)
//                //change (PList.tryAt (i-1) currentState)
//                change (selectedElem currentState)
//                    
//            onEvent "onchange" ["event.target.selectedIndex"] 
//                (fun x -> 
//                    x 
//                        |> List.head 
//                        |> Int32.Parse 
//                        |> cb)
//        
//
//        Incremental.select (AttributeMap.ofList [rOnChange; style "color:black"]) 
//            (
//                alist {
//                    let debug = values;
//                    let domNode = 
//                        values 
//                            |> AList.mapi(fun i x ->
//                                 Incremental.option 
//                                    (attributes x (Mod.force (getDisplayString x))) 
//                                    (AList.ofList [Incremental.text (getDisplayString x)]))
//                    yield! domNode
//                }
//            )