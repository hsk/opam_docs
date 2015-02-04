open Magick
open Imper

let () =
  let img = get_canvas ~width:400 ~height:300 ~color:"#C8E0FF" in

  (* The clouds *)
  List.iter (fun (cx, cy) ->
    draw_ellipse img ~cx ~cy
        ~rx:50 ~ry:20 ~fill_color:(color_of_string "#FFFB") ()
  ) [ (20, 40); (60, 55); (90, 30); (120, 65); (175, 35); ];
  write_image img "test.jpg"



