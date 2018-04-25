
module App

open Aardvark.UI
open Aardvark.UI.Primitives

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Model


module Shader =
    open FShade
    open FShade.InstrinsicAttributes
    open FShade.UniformExtensions
    open Aardvark.SceneGraph.Semantics
    open Aardvark.Application

    type Vertex = {
      [<Position>]    pos     : V4d
      [<Color>]       col     : V4d
      [<Normal>]      n       : V3d
    }

    type UniformScope with
        member x.MyUniform : V4d = uniform?MyUniform

    let vertexShader (v : Vertex) =
      vertex {
          return {
              pos = uniform.ModelViewProjTrafo * v.pos
              col = v.col
              n = uniform.NormalMatrix * v.n
          }
      }

    let fragmentShader (v : Vertex) =
      fragment {
          let col = (v.n * V3d(1.0, 1.0, 1.0)).Normalized
          return {v with col = V4d(col, 0.5)}
      }

//    let fragmentShader (v : Vertex) =
//      let lightPos = V3d(10.0, 10.0, 10.0)
//      fragment {
//          let lDir = lightPos - V3d(v.pos.X, v.pos.Y, v.pos.Z)
//          let dotNL = max 0.0 (V3d.Dot(lDir.Normalized, v.n.Normalized))
//          let col = V3d(uniform.MyUniform) * dotNL
//          let col = col + V3d(0.05, 0.05, 0.05)
//          return {v with col = V4d(col, 1.0)}
//      }

let initialCamera = { 
        CameraController.initial with 
            view = CameraView.lookAt (V3d.III * 3.0) V3d.OOO V3d.OOI
    }

let update (model : Model) (msg : Message) =
    match msg with
        | Camera m -> 
            { model with cameraState = CameraController.update model.cameraState m }
        | CenterScene -> 
            { model with cameraState = initialCamera }

let viewScene (model : MModel) =
    Sg.box (Mod.constant C4b.Green) (Mod.constant Box3d.Unit)
      |> Sg.noEvents
      |> Sg.uniform "MyUniform" (Mod.constant (V4d(0.0, 0.0, 1.0, 1.0)))
      |> Sg.effect [
          Shader.vertexShader |> toEffect
          Shader.fragmentShader |> toEffect
      ]
//     |> Sg.shader {
//            do! DefaultSurfaces.trafo
//            do! DefaultSurfaces.vertexColor
//            do! DefaultSurfaces.simpleLighting
//        }

let view (model : MModel) =

    let renderControl =
        CameraController.controlledControl model.cameraState Camera (Frustum.perspective 60.0 0.1 100.0 1.0 |> Mod.constant) 
                    (AttributeMap.ofList [ style "width: 800px; height:600px"]) 
                    (viewScene model)

    body [] [
        text "Hello 3D"
        br []
        button [onClick (fun _ -> CenterScene)] [text "Center Scene"]
        br []
        renderControl
    ]

// variant with html5 grid layouting (currently not working in our cef)
let view2 (model : MModel) =

    let renderControl =
        CameraController.controlledControl model.cameraState Camera (Frustum.perspective 60.0 0.1 100.0 1.0 |> Mod.constant) 
                    (AttributeMap.ofList [ style "width: 100%; grid-row: 2"]) 
                    (viewScene model)

    body [] [
        div [style "display: grid; grid-template-rows: 40px 1fr; width: 100%; height: 100%" ] [
            div [style "grid-row: 1"] [
                text "Hello 3D"
                br []
                button [onClick (fun _ -> CenterScene)] [text "Center Scene"]
            ]
            renderControl
            br []
            text "use first person shooter WASD + mouse controls to control the 3d scene"
        ]
    ]

let threads (model : Model) = 
    CameraController.threads model.cameraState |> ThreadPool.map Camera


let app =                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
    {
        unpersist = Unpersist.instance     
        threads = threads 
        initial = 
            { 
               cameraState = initialCamera
            }
        update = update 
        view = view
    }
