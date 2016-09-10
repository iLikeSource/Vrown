namespace Vrown.ViewModels

open System
open System.Collections.Generic
open System.Linq
open System.Web
open System.Web.Mvc
open System.Web.Mvc.Ajax
open Newtonsoft.Json
open SVGGenerator

module SubModels = 

    [<CLIMutable>]
    type SvgBlock = {
        Width : int
        Height : int
        Contents : SVGContent.t list
    } 
