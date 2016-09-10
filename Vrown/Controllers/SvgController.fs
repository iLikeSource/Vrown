namespace Vrown.Controllers

open System
open System.Collections.Generic
open System.Linq
open System.Web
open System.Web.Mvc
open System.Web.Mvc.Ajax
open Newtonsoft.Json
open SVGGenerator
open Vrown.Calculation.Cmq
open Vrown.ViewModels.SubModels

type SvgController() =
    inherit Controller()

    [<ChildActionOnly>]
    member this.Index () = 
        let model : SvgBlock = { Width = 400; Height = 400; Contents = Generator.Stub() }
        this.PartialView(model)

