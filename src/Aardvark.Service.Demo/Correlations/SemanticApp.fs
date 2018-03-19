namespace CorrelationDrawing

open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.UI

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SemanticApp = 


  type Action =
    | SetSemantic       of option<string>
    | AddSemantic
    | SemanticMessage   of Semantic.Action
    | SortBy            of SemanticsSortingOption


    ///// INITIAL
  let initial : SemanticApp = {
    semantics         = hmap.Empty
    selectedSemantic  = ""
    semanticsList     = plist.Empty
    sortBy            = SemanticsSortingOption.Label
  }


  ///// convienience functions

  let enableSemantic (s : option<Semantic>) = 
    (Option.map (fun x -> Semantic.update x Semantic.Enable) s)

  let disableSemantic (s : option<Semantic>) = 
    (Option.map (fun x -> Semantic.update x Semantic.Disable) s)

  let getSortedList (list    : hmap<string, Semantic>) 
                    (sortBy  : SemanticsSortingOption) =
    let f = 
      match sortBy with
         | SemanticsSortingOption.Label        -> fun (x : Semantic) -> x.label.text
         | SemanticsSortingOption.Level        -> fun (x : Semantic) -> (sprintf "%03i" x.level)
         | SemanticsSortingOption.GeometryType -> fun (x : Semantic) -> x.geometry.ToString()
         | SemanticsSortingOption.SemanticType -> fun (x : Semantic) -> x.semanticType.ToString()
         | SemanticsSortingOption.Id           -> fun (x : Semantic) -> x.id

    UtilitiesDatastructures.sortedPlistFromHmap list f

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
                        (Semantic.initial id),
                        (sprintf "Semantic%i" (model.semantics.Count + 1)))
    insertSemantic newSemantic model

  let getInitialWithSamples =
    initial
      |> insertSemantic (Semantic.initialHorizon (System.Guid.NewGuid().ToString()))
      |> insertSemantic (Semantic.initialCrossbed (System.Guid.NewGuid().ToString()))
      |> insertSemantic (Semantic.initialGrainSize (System.Guid.NewGuid().ToString()))

  ////// UPDATE 
  let update (model : SemanticApp) (action : Action) =
    match (action) with 
      | SetSemantic sem ->
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

      | SemanticMessage sem ->
        let fUpdate (semO : Option<Semantic>) = 
            match semO with
                | Some s  -> Some(Semantic.update s sem)
                | None    -> None
        let updatedSemantics = HMap.alter model.selectedSemantic fUpdate model.semantics
        {model with semantics     = updatedSemantics
                    semanticsList = getSortedList updatedSemantics model.sortBy}

      | AddSemantic -> insertSampleSemantic model 

      | SortBy sortingOption ->
        {model with sortBy        = sortingOption
                    semanticsList = (getSortedList model.semantics sortingOption)}



  ///// VIEW

  let viewSemantics (model : MSemanticApp) = 
    let domList = 
      alist {                 
        for mSem in model.semanticsList do
          let! domNode = Semantic.view mSem
          yield (tr 
                  ([style UtilitiesGUI.tinyPadding; onClick (fun str -> SetSemantic (Some mSem.id))]) 
                  (List.map (fun x -> x |> UI.map SemanticMessage) domNode))
                
      } 

    Html.SemUi.accordion "Semantics" "File Outline" true [
      table
        ([clazz "ui celled striped selectable inverted table unstackable";
                              style "padding: 1px 5px 1px 5px"]) (
            [thead [][tr[][th[][text "Label"];
                           th[][text "Thickness"];
                           th[][text "Colour"]]];
            Incremental.tbody  (AttributeMap.ofList []) domList]           
        )
    ]

  let app : App<SemanticApp, MSemanticApp, Action> =
      {
          unpersist = Unpersist.instance
          threads   = fun _ -> ThreadPool.empty
          initial   = getInitialWithSamples
          update    = update
          view      = viewSemantics
      }

  let start () = App.start app

