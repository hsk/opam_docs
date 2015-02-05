let _ =
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  let connection = new Curl.handle in
  connection#set_url "http://google.com/";
  connection#perform;
