namespace Vrown.Calculation.Cmq

#if INTERACTIVE
#r "../packages/Newtonsoft.Json.9.0.1/lib/net45/Newtonsoft.Json.dll"
#endif

open Newtonsoft.Json
open Newtonsoft.Json.Serialization

module Cmq =  

    ///  型定義 
    type t = {
        ca : float
        cb : float
        qa : float
        qb : float
        m0 : float
    } 
    with
    
    ///  CMQ荷重を加算する 
    member x.Add (y) = {
        ca = x.ca + y.ca
        cb = x.cb + y.cb
        qa = x.qa + y.qa
        qb = x.qb + y.qb
        m0 = x.m0 + y.m0 
    }
    ///  +演算子
    static member( + ) (x:t, y:t) = x.Add (y)

    ///  空のCMQ
    static member Empty () = {
        ca = 0.0
        cb = 0.0
        qa = 0.0
        qb = 0.0
        m0 = 0.0 
    }
    
    ///  CMQ荷重を合計する 
    static member Sum (xs:t list) = 
        xs |> List.fold (fun dst src -> dst + src) (t.Empty ())


    ///  集中荷重 
    static member ConcentrationLoadCmq (l:float, p:float, x:float) = 
        let (a, b) = (x, l - x) 
        let ca = - p * a        * b ** 2.0 / (l ** 2.0)
        let cb = - p * a ** 2.0 * b        / (l ** 2.0)
        let qa =   p * b ** 2.0 * (3.0 * a +        b) / (l ** 3.0)
        let qb =   p * a ** 2.0 * (       a + 3.0 * b) / (l ** 3.0)
        let m0 =   p * a * b / l 
        { ca = ca; cb = cb; qa = qa; qb = qb; m0 = m0 }

    ///  積分 
    static member Integration (l:float, f:float->float, ?div) = fun (x1, x2) ->
        let div = defaultArg div 1000
        let range = x2 - x1
        let dx    = range / (float div)
        
        //  dx区間に作用する荷重 
        let p (x) = f (x) * dx
        
        //  集中荷重として計算 
        Array.init div (fun i -> x1 + dx * (0.5 + float i))
        |> Array.map (fun x -> t.ConcentrationLoadCmq (p=(p (x)), l=l, x=x))
        |> Array.toList
        |> t.Sum


(** 集中荷重 *)
module ConcentrationLoad = 

     ///  型定義 
     type t = {
         p1 : float;  (* 荷重 *)
         x1 : float   (* 左端から荷重点までの距離 *)
     } with

     ///  CQM荷重に変換 
     member this.ToCmq (length) = 
         Cmq.t.ConcentrationLoadCmq (l=length, p=this.p1, x=this.x1) 

     ///  生成
     static member Create (p1, x1) = { p1 = p1; x1 = x1 }

    


(** 分布荷重 *)
module DistributionLoad = 
     (** 型定義 *)
     type t = {
         p1 : float;  (* 荷重開始点の単位長さあたり荷重 *)
         p2 : float;  (* 荷重終了点の単位長さあたり荷重 *)
         x1 : float;  (* 左端から荷重開始点までの距離 *)
         x2 : float;  (* 左端から荷重終了点までの距離 *)
     } with

     ///  CMQ荷重に変換 
     member this.ToCmq (length:float) = 
         if this.x1 = this.x2 
         then 
             Cmq.t.Empty ()
         else
             let f (x) = this.p1 + (x - this.x1) * (this.p2 - this.p1) / (this.x2 - this.x1)
             Cmq.t.Integration (l=length, f=f) <| (this.x1, this.x2)
     
     ///  生成 
     static member Create (p1, p2, x1, x2) = { p1 = p1; x1 = x1; p2 = p2; x2 = x2 }
    

///  梁荷重 
module BeamLoad =
    ///  型定義 
    type t = 
        | Concentration of ConcentrationLoad.t
        | Distribution  of DistributionLoad.t

    ///  CMQ荷重に変換 
    let ToCmq (length:float) = function
        | Concentration (x) -> x.ToCmq (length)
        | Distribution  (x) -> x.ToCmq (length)


/// 
module Beam = 

    open BeamLoad
    
    type t = {
        length : float
        loads : BeamLoad.t list
    } with
    member this.AddLoad (load:BeamLoad.t) = { this with loads = load :: this.loads }    
    static member Create (length:float) = { length = length; loads = [] }
    
    ///  JSON形式のデータを読み込み 
    static member Deserialize (json:string) = 
        JsonConvert.DeserializeObject<t>(json)
    
    ///  サンプルデータをJSON形式で取得
    static member Sample () = 
        t.Create(5.0).AddLoad(Concentration (ConcentrationLoad.t.Create (p1=10.0, x1=3.0)))
                     .AddLoad(Concentration (ConcentrationLoad.t.Create (p1=15.0, x1=1.0)))
                     .AddLoad(Distribution (DistributionLoad.t.Create (p1=2.0, p2=4.0, x1=0.0, x2=3.0))) 
         
    ///  サンプルデータをJSON形式で取得
    static member SampleJson () = 
        JsonConvert.SerializeObject(t.Sample(), Formatting.Indented)   




    


