namespace Vrown.Controllers

open System
open System.Collections.Generic
open System.Linq
open System.Web
open System.Web.Mvc
open System.Web.Mvc.Ajax
open SVGGenerator

type CmqController() =
    inherit Controller()
    member this.Index () = 
        this.ViewData.["SVGModel"] <- Generator.Stub()
        this.View()


