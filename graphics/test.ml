let _ =
  Graphics.open_graph " 640x480";
  Graphics.display_mode false;
  for x = 0 to 50000 do
    let x = Random.int 640 in
    let y = Random.int 480 in
    Graphics.plot x y
  done;
  let _ = Graphics.wait_next_event [ Graphics.Button_down ] in
  ()

let _ =
  Graphics.set_color (Graphics.rgb 128 255 255);
  Graphics.clear_graph ();
  for x = 0 to 50000 do
    let x = Random.int 640 in
    let y = Random.int 480 in
    Graphics.plot x y
  done;
  let _ = Graphics.wait_next_event [ Graphics.Button_down ] in
  Graphics.close_graph()
