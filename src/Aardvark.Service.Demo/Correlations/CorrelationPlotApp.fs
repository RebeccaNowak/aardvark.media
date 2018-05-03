namespace CorrelationDrawing

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CorrelationPlotApp =
    open Aardvark.Base.Rendering
    open Aardvark.Base.Incremental
    //open Aardvark.Base.Incremental.Operators
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI
    open UtilitiesGUI

    type Action =
      | ToggleSelectLog of option<string>
      | NewLog          
      | TogglePoint     of (V3d * Annotation)
      | FinishLog       
      | DeleteLog       
      | LogMessage      of GeologicalLog.Action

    let initial : CorrelationPlotApp = {
      logs                = PList.empty
      working             = List<(V3d * Annotation)>.Empty
      selectedLog         = None
      creatingNew         = false
    }

    let update (model : CorrelationPlotApp) (annos : plist<Annotation>) (semApp : SemanticApp) (action : Action) = 
      match action, model.creatingNew with
        | ToggleSelectLog oStr, false  -> 
          match (oStr = model.selectedLog) with
            | true  -> {model with selectedLog = None}
            | false -> {model with selectedLog = oStr}
        | NewLog, false          -> {model with creatingNew = true
                                                working     = List<(V3d * Annotation)>.Empty}
        | TogglePoint pa, true   ->
          let onlyVector ((x,y) : (V3d*Annotation)) = x
          let (point, anno) = pa
          let pointIsInList = model.working |> List.contains' (fun (p, a) -> (p.AllEqual(point)))
          //let bar = (List.contains (f pa) (model.working |> List.map f))
          match pointIsInList with  // toggle if same point is in list
            | true  -> let filteredList = (model.working 
                                                |> List.filter  
                                                  (fun (x : (V3d*Annotation)) 
                                                      -> let v = (onlyVector x)
                                                         v.AnyDifferent(point)
                                                  )
                                              )
                       {model with working = filteredList}
            | false -> // only add if no point is selected on this annotation yet
              //match (model.working |> List.contains'' (fun ((_, a) : (V3d*Annotation)) -> a.semanticId) pa) with // WRONG!!!
              match (model.working |> List.contains' (fun ((_, a) : (V3d*Annotation)) -> anno.id = a.id)) with
                | true  -> model
                | false -> 
                  {model with working     = (List.append model.working [pa])}
            
          
        | FinishLog, true        ->
          {model with creatingNew = false
                      logs        = (model.logs.Append 
                                      (GeologicalLog.intial (System.Guid.NewGuid().ToString()) model.working annos semApp))
                      working     = List<(V3d * Annotation)>.Empty}
        | DeleteLog, false       -> model
        | LogMessage m, _        -> model//{model with logs = model.logs.Update m}
        | _,_                    -> model



    let view (model : MCorrelationPlotApp) (semApp : MSemanticApp) =
      let menu =
        let icon =
          alist {
            let! ic =
              (model.creatingNew |> Mod.map (fun n -> 
                                              match n with
                                                | true  -> i [clazz "small yellow plus icon"] [] 
                                                | false -> i [clazz "small plus icon"] []
                                            ))
            yield ic
          }
        div [clazz "ui horizontal inverted menu";
             style "float:top"]
            [
              div [clazz "item"]
                  [Incremental.button (AttributeMap.ofList [clazz "ui small icon button"; onMouseClick (fun _ -> NewLog)]) 
                                       icon
                  ];
              div [clazz "item"]
                  [button [clazz "ui small icon button"; onMouseClick (fun _ -> FinishLog)] 
                          [i [clazz "small check icon"] [] ] |> UtilitiesGUI.wrapToolTip "done"
                  ];
              div [clazz "item"]
                  [button [clazz "ui small icon button"; onMouseClick (fun _ -> DeleteLog)] 
                          [i [clazz "small minus icon"] [] ] |> UtilitiesGUI.wrapToolTip "delete"
                  ]; 
            ]

      let domList =
         alist {            
            for log in model.logs do
              let! sel = model.selectedLog
              let isSelected = 
                match sel with
                  | Some s  -> s = log.id
                  | None    -> false
              let st = 
                match isSelected with
                  | true  -> style (sprintf "%s;%s;%s" UtilitiesGUI.tinyPadding (bgColorStr C4b.Yellow) "color:black")
                  | false -> style UtilitiesGUI.tinyPadding
              yield (Incremental.tr 
                      (AttributeMap.ofList [st; onClick (fun str -> ToggleSelectLog (Some log.id))])
                      (GeologicalLog.view log semApp)
                    )
          }     

      let myCss = [
          { kind = Stylesheet;  name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
          { kind = Stylesheet;  name = "semui-overrides"; url = "semui-overrides.css" }
          { kind = Script;      name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
        ]

      require (myCss) (
        body [] [
          div [] [
            menu
            table
              ([clazz "ui celled striped inverted table unstackable";
                                    style "padding: 1px 5px 1px 5px"]) (
                  [thead [][
                    tr[][th[][text "Name"];
                                  //th[][text "Weight"];
                                  //th[][text "Colour"];
                                  //th[][text "Level"];
  //                               th[][text "Default Geometry Type"];
                                  //th[][text "Semantic Type"]
                    ]
                  ];
                  Incremental.tbody  (AttributeMap.ofList []) domList]           
              )
          ]
        ]
      )