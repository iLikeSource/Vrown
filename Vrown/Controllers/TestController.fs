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

type TestController() =
    inherit Controller()

    let width = 400
    let height = 400

    member this.Index () = 
        let model : SvgBlock = { Width = width; Height = height; Contents = Generator.Stub() }
        this.View(model)

    member this._SVG (index:int) =
        let model : SvgBlock = 
            let contents = 
                let x1 = float (index * 10)                
                let y1 = float (index * 10)
                let x2 = float (index * 30)
                let y2 = float (index * 40)
                SVGContent.line (x1, y1, x2, y2)
                :: (Generator.Stub())
            { Width = width; Height = height; Contents = contents }
        this.PartialView(model)
        