﻿namespace CorrelationDrawing

open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.Application
open Aardvark.UI

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SemanticApp = 


  type Action =
    | SetSemantic       of option<string>
    | AddSemantic
    | DeleteSemantic
    | SemanticMessage   of Semantic.Action
    | SortBy            


    ///// INITIAL
  let initial : SemanticApp = {
    semantics         = hmap.Empty
    selectedSemantic  = ""
    semanticsList     = plist.Empty
    sortBy            = SemanticsSortingOption.Timestamp
  }

  ///// convenience functions Semantics

  let getColor (model : MSemanticApp) (semanticId : IMod<string>) =
    let sem = Mod.bind (fun id -> AMap.tryFind id model.semantics) semanticId
    Mod.bind (fun (se : option<MSemantic>) ->
      match se with
                  | Some s -> s.style.color.c
                  | None -> Mod.constant C4b.Red) sem


  let getThickness (model : MSemanticApp) (semanticId : IMod<string>) =
    let sem = Mod.bind (fun id -> AMap.tryFind id model.semantics) semanticId
    Mod.bind (fun (se : option<MSemantic>) ->
      match se with
                  | Some s -> s.style.thickness.value
                  | None -> Mod.constant 1.0) sem

  let getLabel (model : MSemanticApp) (semanticId : IMod<string>) = 
    let sem = Mod.bind (fun id -> AMap.tryFind id model.semantics) semanticId
    sem
        |> Mod.bind (fun x ->
                          match x with 
                            | Some s -> s.label.text
                            | None -> Mod.constant "-NONE-")

 

  ///// convienience functions II

  let next (e : SemanticsSortingOption) = 
    let plusOneMod (x : int) (m : int) = (x + 1) % m
    let eInt = int e
    enum<SemanticsSortingOption>(plusOneMod eInt 6) // hardcoded :(

  let enableSemantic (s : option<Semantic>) = 
    (Option.map (fun x -> Semantic.update x Semantic.Enable) s)

  let disableSemantic (s : option<Semantic>) = 
    (Option.map (fun x -> Semantic.update x Semantic.Disable) s)


  let sortFunction (sortBy : SemanticsSortingOption) = 
    match sortBy with
      | SemanticsSortingOption.Label        -> fun (x : Semantic) -> x.label.text
      | SemanticsSortingOption.Level        -> fun (x : Semantic) -> (sprintf "%03i" x.level)
      | SemanticsSortingOption.GeometryType -> fun (x : Semantic) -> x.geometry.ToString()
      | SemanticsSortingOption.SemanticType -> fun (x : Semantic) -> x.semanticType.ToString()
      | SemanticsSortingOption.Id           -> fun (x : Semantic) -> x.id
      | SemanticsSortingOption.Timestamp    -> fun (x : Semantic) -> x.timestamp
      | _                                   -> fun (x : Semantic) -> x.timestamp

  let getSortedList (list    : hmap<string, Semantic>) 
                    (sortBy  : SemanticsSortingOption) =
    UtilitiesDatastructures.sortedPlistFromHmap list (sortFunction sortBy)

  let insertSemantic (s : Semantic) (model : SemanticApp) = 
    let newSemantics = (model.semantics.Add(s.id, s)
        |> HMap.alter model.selectedSemantic disableSemantic
        |> HMap.alter s.id enableSemantic)

    {model with selectedSemantic  = s.id
                semantics         = newSemantics
                semanticsList     = getSortedList newSemantics model.sortBy
    }


  let insertSampleSemantic (model : SemanticApp) = 
    let id = System.Guid.NewGuid().ToString()
    let newSemantic = Semantic.Lens._labelText.Set(
                        (Semantic.initial id),"NewSemantic")
    insertSemantic newSemantic model

  let getInitialWithSamples =
    initial
      |> insertSemantic (Semantic.initialHorizon0 (System.Guid.NewGuid().ToString()))
      |> insertSemantic (Semantic.initialHorizon1 (System.Guid.NewGuid().ToString()))
      |> insertSemantic (Semantic.initialHorizon2 (System.Guid.NewGuid().ToString()))
      |> insertSemantic (Semantic.initialHorizon3 (System.Guid.NewGuid().ToString()))
      |> insertSemantic (Semantic.initialHorizon4 (System.Guid.NewGuid().ToString()))
      |> insertSemantic (Semantic.initialCrossbed (System.Guid.NewGuid().ToString()))
      |> insertSemantic (Semantic.initialGrainSize (System.Guid.NewGuid().ToString()))

  ////// UPDATE 
  let update (model : SemanticApp) (action : Action) =
    match (action) with 
      | SetSemantic sem       ->
        match sem with
          | Some s  ->
              let updatedSemantics = 
                model.semantics
                  |> HMap.alter model.selectedSemantic disableSemantic
                  |> HMap.alter s enableSemantic
                      
              {model with selectedSemantic  = s
                          semanticsList     = getSortedList updatedSemantics model.sortBy 
                          semantics         = updatedSemantics}
          | None    -> model

      | SemanticMessage sem   ->
        let fUpdate (semO : Option<Semantic>) = 
            match semO with
                | Some s  -> Some(Semantic.update s sem)
                | None    -> None
        let updatedSemantics = HMap.alter model.selectedSemantic fUpdate model.semantics
        {model with semantics     = updatedSemantics
                    semanticsList = getSortedList updatedSemantics model.sortBy}

      | AddSemantic     -> insertSampleSemantic model 

      | DeleteSemantic  ->
        let getAKey (m : hmap<string, 'a>) =
          m |> HMap.toSeq |> Seq.map fst |> Seq.first

        let rem =
          model.semantics
            |> HMap.remove model.selectedSemantic

        match getAKey rem with
          | Some k  -> 
            let updatedSemantics = (rem |> HMap.alter k enableSemantic)
            {model with 
              semantics = updatedSemantics 
              semanticsList = getSortedList updatedSemantics model.sortBy
              selectedSemantic = k
            }
          | None   -> model



      | SortBy          ->
        let newSort = next model.sortBy
        {model with sortBy = newSort
                    semanticsList = 
                      model.semanticsList
                        |> PList.toSeq
                        |> Seq.sortBy (sortFunction newSort)
                        |> PList.ofSeq
        }


      | _                     -> model



  ///// VIEW

  let viewSemantics (model : MSemanticApp) = 
    let menu = 
      div [clazz "ui horizontal inverted menu";
           style "width:100%; height: 10%; float:middle; vertical-align: middle"][
        div [clazz "item"]
            [button [clazz "ui small icon button"; onMouseClick (fun _ -> AddSemantic)] 
                    [i [clazz "plus icon"] [] ] |> UtilitiesGUI.wrapToolTip "add"];
        div [clazz "item"]
            [button [clazz "ui small icon button"; onMouseClick (fun _ -> DeleteSemantic)] 
                    [i [clazz "minus icon"] [] ] |> UtilitiesGUI.wrapToolTip "delete"];
        div [clazz "item"] [
          button 
            [clazz "ui small icon button"; style "width: 20ch; text-align: left"; onMouseClick (fun _ -> SortBy;)]
            [Incremental.text (Mod.map (fun x -> sprintf "sort: %s" (string x)) model.sortBy)]
        ]  
      ]
    
    let domList = 
      alist {                 
        for mSem in model.semanticsList do
          let! domNode = Semantic.view mSem
          yield (tr 
                  ([style UtilitiesGUI.tinyPadding; onClick (fun str -> SetSemantic (Some mSem.id))]) 
                  (List.map (fun x -> x |> UI.map SemanticMessage) domNode))
                
      } 

    let myCss = [
              { kind = Stylesheet; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
              { kind = Stylesheet; name = "semui-overrides"; url = "semui-overrides.css" }
              { kind = Script; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
            ]

    require (myCss) (
      body [clazz "ui"; style "background: #1B1C1E;position:fixed;width:100%"] [
        div [] [
          menu
          //Html.SemUi.accordion "Semantics" "File Outline" true [

          table
            ([clazz "ui celled striped selectable inverted table unstackable";
                                  style "padding: 1px 5px 1px 5px"]) (
                [thead [][tr[][th[][text "Label"];
                                th[][text "Thickness"];
                                th[][text "Colour"];
                                th[][text "Level"];
                                th[][text "Geometry Type"];
                                th[][text "Semantic Type"]]];
                Incremental.tbody  (AttributeMap.ofList []) domList]           
            )
        ]])

  let app : App<SemanticApp, MSemanticApp, Action> =
      {
          unpersist = Unpersist.instance
          threads   = fun _ -> ThreadPool.empty
          initial   = getInitialWithSamples
          update    = update
          view      = viewSemantics
      }

  let start () = App.start app

