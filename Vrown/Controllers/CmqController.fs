namespace Vrown.Controllers

open System
open System.Collections.Generic
open System.Linq
open System.Web
open System.Web.Mvc
open System.Web.Mvc.Ajax
open SVGGenerator
open Vrown.Calculation.Cmq

type Action = 
    | Add of BeamLoad.t 

type ViewModel = {
    beam : Beam.t
    actions : Action list        
}

type JsonModel = {
    load_type : int
    p1 : float
    p2 : float
    x1 : float
    x2 : float
}

type CmqController() =
    inherit Controller()

    let width   = 400
    let height  = 400
    let marginL =  40 
    let marginR =  40 
    let marginT =  40 
    let marginB =  40 

    let ofRecord (x:JsonModel) (viewModel:ViewModel) = 
        match x.load_type with
        | 1 -> 
            let load = BeamLoad.t.Concentration ({ p1=x.p1; x1=x.x1 })
            let beam = viewModel.beam.AddLoad(load)
            { beam = beam; actions = Add (load) :: viewModel.actions }
        | 2 -> 
            let load = BeamLoad.t.Distribution ({ p1=x.p1; p2=x.p2; x1=x.x1; x2=x.x2 })
            let beam = viewModel.beam.AddLoad(load)
            { beam = beam; actions = Add (load) :: viewModel.actions }
        | _ -> 
            viewModel
             

    [<NonAction>]
    member this.Beam (l) = 
        { beam = Beam.t.Create (l);
          actions = [] }

    [<NonAction>]
    member this.DoAction (beam:Beam.t, act:Action) = 
        match act with
        | Add (load) -> beam.AddLoad (load)

    [<NonAction>]
    member this.DoActions (viewModel:ViewModel, offset) = 
        let actions = viewModel.actions
        let length = List.length (actions) - offset
        let beam =
            actions
            |> List.rev
            |> List.mapi (fun i act -> (i, act))
            |> List.choose (fun (i, act) -> if i < length then Some (act) else None)
            |> List.fold (fun beam act -> this.DoAction (beam, act)) viewModel.beam
        { viewModel with beam = beam }
    
    [<NonAction>]
    member this.DoActions (viewModel:ViewModel) = 
        this.DoActions (viewModel, 0)
         
    [<NonAction>]
    member this.Concentration (viewModel:ViewModel, p1:float, x1:float) = 
        let beamLoad = BeamLoad.Concentration (ConcentrationLoad.t.Create (p1, x1))
        { viewModel with actions = Add (beamLoad) :: viewModel.actions }
        |> this.DoActions

    [<NonAction>]
    member this.Distribution (viewModel:ViewModel, p1:float, p2:float, x1:float, x2:float) = 
        let beamLoad = BeamLoad.Distribution (DistributionLoad.t.Create (p1, p2, x1, x2))
        { viewModel with actions = Add (beamLoad) :: viewModel.actions }
        |> this.DoActions
    
    [<NonAction>]
    member this.Magnify (length) = 
        float (width - marginL - marginR) / length         
        
    [<NonAction>]
    member this.Projection magnify (x, y) = 
        let (ox, oy) = (float (width / 2), float (height / 2))
        (ox + x * magnify, oy + y * magnify)
        

    [<NonAction>]
    member this.DrawBeamLine (viewModel:ViewModel) (contents:SVGContent.t list) = 
        let length   = viewModel.beam.length
        let (ox, oy) = (float (width / 2), float (height / 2))
        let magnify  = this.Magnify length         
        let (x1, y1) = this.Projection magnify (- 0.5 * length, 0.0)
        let (x2, y2) = this.Projection magnify (  0.5 * length, 0.0)
        SVGContent.line (x1, oy, x2, oy) :: contents

    [<NonAction>]
    member this.DrawConcentrationLoad (viewModel:ViewModel) (contents:SVGContent.t list) = 
        let length   = viewModel.beam.length
        let (ox, oy) = (float (width / 2), float (height / 2))
        let magnify  = this.Magnify length         
        let arrowLength = 50.0
        viewModel.beam.loads
        |> List.choose (fun x -> 
            match x with
            | BeamLoad.Concentration (cLoad) ->
                let (x, y) = this.Projection magnify (- 0.5 * length + cLoad.x1, 0.0)
                Some { SVGLine.create (x, y - arrowLength, x, y) 
                        with arrow = true; 
                             stroke_width = 1;
                             stroke = "blue" }
            | _ -> None
        )
        |> List.map (fun x -> SVGContent.Line (x))
        |> List.append contents 
    
    [<NonAction>]
    member this.Draw (viewModel:ViewModel) = 
        List.empty
        |> this.DrawBeamLine (viewModel) 
        |> this.DrawConcentrationLoad (viewModel) 


    ///  クライアントから送られてきたJSONデータを処理する
    [<HttpPost>]
    member this.Ajax_Post (length, loads:JsonModel array) = 
        if this.Request.IsAjaxRequest () then
            let viewModel =
                loads
                |> Array.fold (fun beam load ->
                    ofRecord load beam
                ) ({ beam = Beam.t.Create(length); actions = [] })
            this.Draw (viewModel) 
            |> Generator.dump    
            |> this.Content 
        else
            this.Content( "このアクションには直接アクセスできません" )

    member this.Index () = 
        this.ViewData.["Width"]  <- width
        this.ViewData.["Height"] <- height
        
        let viewModel = 
            { beam    = Beam.t.Sample();
              actions = [] } 
        
        this.ViewData.["SVGModel"] <- this.Draw (viewModel) |> Generator.dump
        this.View()


