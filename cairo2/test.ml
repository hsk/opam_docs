let width  = 400
let height = 400

let main = 
  (* Setup Cairo *)
  let surface = Cairo.Image.create Cairo.Image.ARGB32 width height in
  let ctx = Cairo.create surface in

  (* Set thickness of brush *)
  Cairo.set_line_width ctx 15. ;

  (* Draw out the triangle using absolute coordinates *)
  Cairo.move_to     ctx   200.  100. ;
  Cairo.line_to     ctx   300.  300. ;
  Cairo.rel_line_to ctx (-200.)   0. ;
  Cairo.Path.close ctx ;

  (* Apply the ink *)
  Cairo.stroke ctx ;

  (* Output a PNG file *)
  Cairo.PNG.write surface "triangle.png"

