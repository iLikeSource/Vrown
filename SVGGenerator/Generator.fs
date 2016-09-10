namespace SVGGenerator


module SVGHelper = 
    
    let intToString (x) = Printf.sprintf "%d" x
    let floatToString (x) = Printf.sprintf "%.0f" x
    let pointsToString (xs) = 
        xs |> List.fold (fun dst (x, y) ->
            dst + Printf.sprintf "%.0f,%.0f " x y
        ) "" 

    let systemColors = 
        [ "black"; "white"; "red"; "purple"; "green"; "yellow"; "blue" ]
     
    let toSVG (elementName:string, value:string) (attrs:(string * string) list) = 
        let attrString = 
            attrs |> List.fold (fun dst (name, value) -> 
                dst + (Printf.sprintf "%s=\"%s\" " name value)
            ) "" 
        Printf.sprintf "<%s " elementName + attrString + Printf.sprintf ">" + 
        value +
        Printf.sprintf "</%s>" elementName

module SVGMarker = 
    open SVGHelper

    type orient = Angle of float | Auto

    type t = {
        id : string
        units : string
        width : int
        height : int
        refX : int
        refY : int
        orient : orient
    }
    
    let orientToString = function
        | Angle x -> floatToString x
        | Auto -> "auto"

    let map (t:t) = 
        [ ("id", t.id);
          ("markerUnits", t.units);
          ("markerWidth", intToString t.width);
          ("markerHeight", intToString t.height);
          ("refX", intToString t.refX);
          ("refY", intToString t.refY);
          ("viewBox", "0 0 40 40");
          ("orient", orientToString t.orient) ]

    let arrowHeadId (fill) = 
        Printf.sprintf "arrowhead-%s" fill

    let arrowHeadMarker (fill:string) = 
        { id = arrowHeadId (fill);
          units = "strokeWidth";
          width = 40;
          height = 40;
          refX = 40;
          refY = 20;
          orient = Auto }
        |> map
        |> toSVG ("marker", Printf.sprintf "<polygon points=\"20,20 20,10 40,20 20,30 \" fill=\"%s\">" fill)



    let defs () =
        SVGHelper.systemColors
        |> List.map (fun color ->
            "<defs>" + "\r\n" +
            arrowHeadMarker(color) + "\r\n" +
            "</defs>" + "\r\n" 
        )
        |> List.fold (fun dst src -> dst + src) ""


module SVGLine = 
    open SVGHelper

    type t = {
        x1 : float
        y1 : float
        x2 : float
        y2 : float
        stroke : string
        stroke_width : int
        display : string
        arrow : bool
    } 

    let map (t:t) =
        let lineMap = 
            [ ("x1", floatToString t.x1); 
              ("y1", floatToString t.y1);
              ("x2", floatToString t.x2); 
              ("y2", floatToString t.y2);
              ("stroke", t.stroke);
              ("stroke-width", intToString t.stroke_width);
              ("display", t.display) ]
        if t.arrow then 
            lineMap 
            |> List.append [ ("marker-end", 
                              Printf.sprintf "url(#%s)" 
                              <| SVGMarker.arrowHeadId(t.stroke)) ]
        else
            lineMap

    let toSVG (t:t) =
        map (t) |> SVGHelper.toSVG ("line", "")

    let create (x1, y1, x2, y2) = 
        { x1 = x1; y1 = y1; x2 = x2; y2 = y2; stroke = "black"; stroke_width = 3; display = "inline"; arrow = false }
    

module SVGRect = 
    open SVGHelper
    
    type t = {
        x : float
        y : float
        height : float 
        width : float 
        fill : string
        stroke : string
        stroke_width : int
        display : string
    }

    let map (t:t) = 
        [ ("x", floatToString t.x); 
          ("y", floatToString t.y);
          ("width", floatToString t.width); 
          ("height", floatToString t.height);
          ("fill", t.fill); 
          ("stroke", t.stroke);
          ("stroke-width", intToString t.stroke_width);
          ("display", t.display) ]

    let toSVG (t:t) = 
        map (t) |> SVGHelper.toSVG ("rect", "")
    
    let create (x, y, width, height) = 
        { x = x; y = y; width = width; height = height; stroke = "black"; stroke_width = 1; fill = "white"; display = "inline" }


module SVGPolygon = 
    open SVGHelper    

    type t = {
        points : (float * float) list
        fill : string
        stroke : string
        stroke_width : int
        display : string
    }

    let toSVG (t:t) = 
        [ ("points", pointsToString t.points);
          ("fill", t.fill);
          ("stroke", t.stroke);
          ("stroke_width", intToString t.stroke_width);
          ("display", t.display) ]
        |> SVGHelper.toSVG ("polygon", "")

    let create (points:(float*float) list) = 
        { points = points; stroke = "black"; stroke_width = 1; fill = "white"; display = "inline" }

    
        


module SVGContent = 
    
    type t = 
        | Line of SVGLine.t
        | Rect of SVGRect.t
        | Polygon of SVGPolygon.t
        | Group of string * (t list)

    let line (x1, y1, x2, y2) : t = Line (SVGLine.create (x1, y1, x2, y2))
    let rect (x, y, width, height) : t = Rect (SVGRect.create (x, y, width, height))
    

    let rec expand = function
        | Group (name, xs) -> Group (name, xs |> List.map expand)  
        | content -> content 

    let arrow (is_arrow) = function 
        | Line x -> Line { x with arrow = is_arrow }
        | content -> content 

    let rec stroke (s) = function
        | Line x -> Line { x with stroke = s }
        | Rect x -> Rect { x with stroke = s }
        | Polygon x -> Polygon { x with stroke = s }
        | Group (name, xs) -> 
            Group (name, xs |> List.map (stroke s)) 
    
    let rec strokeWidth (s) = function
        | Line x -> Line { x with stroke_width = s }
        | Rect x -> Rect { x with stroke_width = s }
        | Polygon x -> Polygon { x with stroke_width = s }
        | Group (name, xs) -> Group (name, xs |> List.map (strokeWidth s)) 
    
    let rec fill (s) = function
        | Line _ as line -> line 
        | Rect x -> Rect { x with fill = s }
        | Polygon x -> Polygon { x with fill = s }
        | Group (name, xs) -> Group (name, xs |> List.map (fill s)) 
    
    let rec display (s) = function
        | Line x -> Line { x with display = s }
        | Rect x -> Rect { x with display = s }
        | Polygon x -> Polygon { x with display = s }
        | Group (name, xs) -> 
            Group (name, xs |> List.map (display s)) 
        

    let grouping (s) (contents:t list) = Group (s, contents)  
        
    let rec toSVG = function
        | Line x -> SVGLine.toSVG(x)
        | Rect x -> SVGRect.toSVG(x)
        | Polygon x -> SVGPolygon.toSVG(x)
        | Group (_, xs) -> 
            "<g>\r\n" +  
            (xs |> List.map toSVG |> List.reduce (fun dst src -> dst + "\r\n" + src)) +
            "\r\n</g>"


module Generator =
    
    open SVGContent

    let dump(contents:SVGContent.t list) = 
        let svgElements =
            contents 
            |> List.map toSVG
            |> List.reduce (fun dst src -> dst + "\r\n" + src)       
            |> Printf.sprintf "%s%s" (SVGMarker.defs())
        SVGMarker.defs () + svgElements

    let Stub() = 
        [ line (10.0, 10.0, 150.0, 200.0) |> display "none"; 
          line (30.0, 100.0, 100.0, 50.0) |> display "none";
          line (30.0, 100.0, 100.0, 150.0) |> arrow true;
          rect (50.0, 20.0, 40.0, 20.0) |> fill "rgba(0,0,200,0.5)" ]
        