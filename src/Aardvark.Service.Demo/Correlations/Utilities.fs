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
                        values |> AList.mapi(fun i x -> Incremental.option (attributes (Mod.force (f x))) (AList.ofList [Incremental.text (f x)]))
                           // |> AList.mapi(fun _ x -> Incremental.option  (Mod.map (fun y ->  (attributes y)) (f x)) ( (AList.ofList [Mod.map (fun y -> (text y)) (f x)] ) ))
                    yield! domNode
                }
            )