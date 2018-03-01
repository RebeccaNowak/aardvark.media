namespace CorrelationDrawing

open System
open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.UI

module CorrelationUtilities =
    let alistFromAMap (input : amap<_,'a>) : alist<'a> = input |> AMap.toASet |> AList.ofASet |> AList.map snd 


    let plistFromHMap (input : hmap<_,'a>) : plist<'a> = input |> HMap.toSeq |> PList.ofSeq |> PList.map snd 

    let sortedPlistFromHmap (input : hmap<_,'a>) (projection : ('a -> 'b)) : plist<'a> =
        input 
            |> HMap.toSeq 
            |> Seq.map snd
            |> Seq.sortBy projection
            |> PList.ofSeq 

    let colorToHexStr (color : C4b) = 
        let bytes = [| color.R; color.G; color.B |]
        let str =
            bytes 
                |> (Array.map (fun (x : byte) -> System.String.Format("{0:X2}", x)))
                |> (String.concat System.String.Empty)
        String.concat String.Empty ["#";str] 

    let bgColorAttr (color : C4b) =
        style (sprintf "background: %s" (colorToHexStr color))

    let noPadding  = "padding: 0px 0px 0px 0px"
    let tinyPadding  = "padding: 1px 1px 1px 1px"

    let wrapToolTip (text:string) (dom:DomNode<'a>) : DomNode<'a> =

        let attr = 
            [attribute "title" text
             attribute "data-position" "top center"
             attribute "data-variation" "mini" ] 
                |> AttributeMap.ofList
                |> AttributeMap.union dom.Attributes                
                
        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (       
            dom.WithAttributes attr     
        )

    let wrapToolTipRight (text:string) (dom:DomNode<'a>) : DomNode<'a> =

        let attr = 
            [ attribute "title" text
              attribute "data-position" "right center"
              attribute "data-variation" "mini"] 
                |> AttributeMap.ofList
                |> AttributeMap.union dom.Attributes                
                
        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (       
            dom.WithAttributes attr     
        )

    let wrapToolTipBottom (text:string) (dom:DomNode<'a>) : DomNode<'a> =

        let attr = 
            [ attribute "title" text
              attribute "data-position" "bottom center"
              attribute "data-variation" "mini"] 
                |> AttributeMap.ofList
                |> AttributeMap.union dom.Attributes                
                
        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (       
            dom.WithAttributes attr     
        )


    let dropDownList (values : alist<'a>)(selected : IMod<Option<'a>>) (change : Option<'a> -> 'msg) (f : 'a ->string)  =

        let attributes (name : string) =
            AttributeMap.ofListCond [
                always (attribute "value" (name))
                onlyWhen (
                        selected 
                            |> Mod.map (
                                fun x -> 
                                    match x with
                                        | Some s -> name = f s
                                        | None   -> name = "-None-"
                            )) (attribute "selected" "selected")
            ]

        let ortisOnChange  = 
            let cb (i : int) =
                let currentState = values.Content |> Mod.force
                change (PList.tryAt (i-1) currentState)
                    
            onEvent "onchange" ["event.target.selectedIndex"] (fun x -> x |> List.head |> Int32.Parse |> cb)

        Incremental.select (AttributeMap.ofList [ortisOnChange; style "color:black"]) 
            (
                alist {
                    yield Incremental.option (attributes "-None-") (AList.ofList [text "-None-"])
                    yield! values |> AList.mapi(fun i x -> Incremental.option (attributes (f x)) (AList.ofList [text (f x)]))
                }
            )


    let dropDownList' (values : alist<'a>)(selected : IMod<IMod<Option<'a>>>)
         (change : Option<'a> -> 'msg) (f : 'a -> string)  =
        let existsSelected =
            adaptive {
                let! msel = selected
                let! s = msel

                return match s with
                        | Some s -> true
                        | None -> false
            }

        let attributes (name : string) =
                AttributeMap.ofListCond [
                    always (attribute "value" (name))
                    onlyWhen (existsSelected
//                                |> Mod.map (
//                                    fun x -> 
//                                        match x with
//                                            | Some s -> name = f s
//                                            | None   -> name = "-None-"
                                ) (attribute "selected" "selected")
                ]
            

        let ortisOnChange  = 
            let cb (i : int) =
                let currentState = values.Content |> Mod.force
                change (PList.tryAt (i-1) currentState)
                    
            onEvent "onchange" ["event.target.selectedIndex"] (fun x -> x |> List.head |> Int32.Parse |> cb)

        Incremental.select (AttributeMap.ofList [ortisOnChange; style "color:black"]) 
            (
                alist {
                    yield Incremental.option (attributes "-None-") (AList.ofList [text "-None-"])
                    // |> AList.mapi((f x) |> Mod.map(fun (y : IMod<'a>) -> fun i x -> Incremental.option (attributes y AList.ofList [text y])))
                    // let domNode = values |> AList.mapi(fun i x -> Incremental.option (attributes (f x)) (AList.ofList [text (f x)]))
                    let domNode = 
                        values |> AList.mapi(fun i x -> Incremental.option (attributes (f x)) (AList.ofList [text (f x)]))
                           // |> AList.mapi(fun _ x -> Incremental.option  (Mod.map (fun y ->  (attributes y)) (f x)) ( (AList.ofList [Mod.map (fun y -> (text y)) (f x)] ) ))
                    yield! domNode
                }
            )

    let dropDownList'' (values : alist<'a>)(selected : IMod<IMod<Option<'a>>>)
         (change : Option<'a> -> 'msg) (f : 'a -> IMod<string>)  =
        let existsSelected =
            adaptive {
                let! msel = selected
                let! s = msel

                return match s with
                        | Some s -> true
                        | None -> false
            }

        let attributes (name : string) =
            let notSelected = (attribute "value" (name))
            let selected = (attribute "selected" "selected")
            let attrMap = 
                AttributeMap.ofListCond [
                    always (notSelected)
                    onlyWhen (existsSelected) (selected)
                ]
            let debug = Mod.force attrMap.Content
            attrMap
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
            

        let ortisOnChange  = 
            let cb (i : int) =
                let currentState = values.Content |> Mod.force
                let selectedElem = PList.tryAt (i)
                //change (PList.tryAt (i-1) currentState)
                change (selectedElem currentState)
                    
            onEvent "onchange" ["event.target.selectedIndex"] 
                (fun x -> 
                    x 
                        |> List.head 
                        |> Int32.Parse 
                        |> cb)
        

        Incremental.select (AttributeMap.ofList [ortisOnChange; style "color:black"]) 
            (
                alist {
                    //yield Incremental.option (attributes "-None-") (AList.ofList [text "-None-"])
                    // |> AList.mapi((f x) |> Mod.map(fun (y : IMod<'a>) -> fun i x -> Incremental.option (attributes y AList.ofList [text y])))
                    // let domNode = values |> AList.mapi(fun i x -> Incremental.option (attributes (f x)) (AList.ofList [text (f x)]))
                    let domNode = 
                        values |> AList.mapi(fun i x -> Incremental.option (attributes (Mod.force (f x))) (AList.ofList [Incremental.text (f x)]))
                           // |> AList.mapi(fun _ x -> Incremental.option  (Mod.map (fun y ->  (attributes y)) (f x)) ( (AList.ofList [Mod.map (fun y -> (text y)) (f x)] ) ))
                    yield! domNode
                }
            )


    let dropDownListR (values : alist<'a>)(selected : IMod<IMod<Option<'a>>>)
         (change : Option<'a> -> 'msg) (getDisplayString : 'a -> IMod<string>) 
         (getIsSelected : 'a -> IMod<bool>) =
//        let isSelected  =
//            adaptive {
//                let! msel = selected
//                let! s = msel
//
//                match s with
//                        | Some s -> let! mb = (getIsSelected s)
//                                    return mb
//                        | None -> return false
//            }


        let attributes (value : 'a) (name : string) =
            let notSelected = (attribute "value" (name))
            let selAttr = (attribute "selected" "selected")
            let attrMap = 
                AttributeMap.ofListCond [
                    always (notSelected)
                    onlyWhen (getIsSelected value) (selAttr)
                ]
            let debug = Mod.force attrMap.Content
            attrMap
            

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

        let rOnChange  = 
            let cb (i : int) =
                let currentState = values.Content |> Mod.force
                let selectedElem = PList.tryAt (i)
                //change (PList.tryAt (i-1) currentState)
                change (selectedElem currentState)
                    
            onEvent "onchange" ["event.target.selectedIndex"] 
                (fun x -> 
                    x 
                        |> List.head 
                        |> Int32.Parse 
                        |> cb)
        

        Incremental.select (AttributeMap.ofList [rOnChange; style "color:black"]) 
            (
                alist {
                    let debug = values;
                    let domNode = 
                        values 
                            |> AList.mapi(fun i x ->
                                 Incremental.option 
                                    (attributes x (Mod.force (getDisplayString x))) 
                                    (AList.ofList [Incremental.text (getDisplayString x)]))
                    yield! domNode
                }
            )


//    let dropDownList''' (values : alist<'a>)(selected : IMod<IMod<Option<'a>>>)
//         (change : IMod<Option<'a>> -> 'msg) (f : 'a -> IMod<string>)  =
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
//                let currentState = values.Content
//                let getSelectedElem (mlst : IMod<plist<'a>>) =
//                    adaptive {
//                        let! lst =  mlst
//                        return PList.tryAt (i-1) lst
//                    }
//                //change (PList.tryAt (i-1) currentState)
//                change (getSelectedElem currentState)
//                    
//            onEvent "onchange" ["event.target.selectedIndex"] 
//                (fun x -> 
//                    x 
//                        |> List.head 
//                        |> Int32.Parse 
//                        |> cb)
//
//        Incremental.select (AttributeMap.ofList [ortisOnChange; style "color:black"]) 
//            (
//                alist {
//                    yield Incremental.option (attributes "-New-") (AList.ofList [text "-New-"])
//                    // |> AList.mapi((f x) |> Mod.map(fun (y : IMod<'a>) -> fun i x -> Incremental.option (attributes y AList.ofList [text y])))
//                    // let domNode = values |> AList.mapi(fun i x -> Incremental.option (attributes (f x)) (AList.ofList [text (f x)]))
//                    let domNode = 
//                        values |> AList.mapi(fun i x -> Incremental.option (attributes (Mod.force (f x))) (AList.ofList [Incremental.text (f x)]))
//                           // |> AList.mapi(fun _ x -> Incremental.option  (Mod.map (fun y ->  (attributes y)) (f x)) ( (AList.ofList [Mod.map (fun y -> (text y)) (f x)] ) ))
//                    yield! domNode
//                }
//            )
//
//
//    let dropDownListR (values : alist<'a>)(selected : IMod<IMod<Option<'a>>>)
//         (changeNew : Option<'a> -> 'msg) (changePrevious : Option<'a> -> 'msg) (f : 'a -> IMod<string>) (prevId : IMod<string>) (selId : IMod<string>) =
//
//        let attributes (name : string) (isSelected : IMod<bool>) =
//            let notSelected = (attribute "value" (name))
//            let selected = (attribute "selected" "selected")
//            let attrMap = 
//                AttributeMap.ofListCond [
//                    always (notSelected)
//                    onlyWhen (isSelected) (selected)
//                ]
//            let debug = Mod.force attrMap.Content
//            attrMap
//            
//
//        let ortisOnChange  = 
//            let cb (i : int) =
//                let currentState = values.Content |> Mod.force
//                let selectedElem = PList.tryAt (i)
//                //change (PList.tryAt (i-1) currentState)
//                
//                changeNew (selectedElem currentState)
//                    
//            onEvent "onchange" ["event.target.selectedIndex"] 
//                (fun x -> 
//                    x 
//                        |> List.head 
//                        |> Int32.Parse 
//                        |> cb)
//
//        Incremental.select (AttributeMap.ofList [ortisOnChange; style "color:black"]) 
//            (
//                alist {
//                    //yield Incremental.option (attributes "-New-" (Mod.constant false)) (AList.ofList [text "-New-"])
//                    let domNode = 
//                        let isSelected (sem : MSemantic) = (sem.id = selId)
//                        values |> AList.mapi(fun i x ->
//                            Incremental.option 
//                                (attributes (Mod.force (f x)) (Mod.constant (isSelected x))) (AList.ofList [Incremental.text (f x)]))
//                    yield! domNode
//                }
//            )


