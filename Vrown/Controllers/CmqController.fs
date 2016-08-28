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
    beam : Beam.t;
    actions : Action list;
    offset : int        
}

type CmqController() =
    inherit Controller()


    [<NonAction>]
    member this.Beam (l) = 
        { beam = Beam.t.Create (l);
          actions = [];
          offset = 0 }

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
        { viewModel with beam = beam; offset = offset }
    
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


    member this.Index () = 
        this.ViewData.["SVGModel"] <- Generator.Stub()
        this.View()


