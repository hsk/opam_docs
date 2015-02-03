let _ =
  let img = new OImages.rgb24 20 20 in
  img#save "aa.png" None [];
  img#destroy

let _ =
  let img = new OImages.rgb24_filled 20 20 {Color.r=255;Color.g=0;Color.b=128} in
  img#save "bb.png" None [];
  img#destroy

let _ =
  let rgb24 = Rgb24.create 20 20 in
  let img = OImages.make (Images.Rgb24 rgb24) in
  img#save "cc.png" None [];
  img#destroy

let _ =
  let rgb24 = Rgb24.make 20 20 {Color.r=255;Color.g=0;Color.b=0} in
  let img = OImages.make (Images.Rgb24 rgb24) in
  img#save "dd.png" None [];
  img#destroy

module DrawString = struct
  module FtDraw = Fttext.Make(Rgb24);;

  let draw_string bitmap str x y =
    let library = Freetype.init () in
    let face, face_info = Freetype.new_face library "micap.ttf" 0 in
    Freetype.set_char_size face 18.0 18.0 72 72;
    let str = Fttext.unicode_of_latin str in
    let x1,y1,x2,y2 = Fttext.size face str in
    FtDraw.draw_text face Fttext.func_darken_only bitmap
        (- (truncate x1)) (truncate y2) str

  let show_image img x y =
    Graphics.open_graph "";
    let gr_img = Graphics.make_image (Graphic_image.array_of_image img) in
    Graphics.draw_image gr_img x y;
    let _ = Graphics.wait_next_event [ Graphics.Button_down ] in
    ()

  let _ =
    let rgb24 = Rgb24.make 200 200 {Color.r=255;Color.g=0;Color.b=0} in
    draw_string rgb24 "test" 0 0;
    show_image (Images.Rgb24 rgb24) 200 200;
    let img = OImages.make (Images.Rgb24 rgb24) in
    img#save "dd.png" None [];
    img#destroy
end
