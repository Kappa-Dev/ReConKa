(* ReConKa
   Copyright 2017 Harvard Medical School

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 3 as
   published by the Free Software Foundation. *)

let file = ref ""

let usage =
  Sys.argv.(0) ^
  " replay a Kappa simulation trace and give connectivity information on it"

let options = []

let replay fname =
  let desc = open_in fname in
  let lex_buf = Lexing.from_channel desc in
  let lex_st = Yojson.init_lexer ~fname () in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = Yojson.Basic.read_lcurl lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let ident = Yojson.Basic.read_ident lex_st lex_buf in
  let () = assert (ident = "env") in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = Yojson.Basic.read_colon lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let env = Model.of_yojson
      (Yojson.Basic.read_json lex_st lex_buf) in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = Yojson.Basic.read_comma lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let ident = Yojson.Basic.read_ident lex_st lex_buf in
  let () = assert (ident = "trace") in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = Yojson.Basic.read_colon lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let _ = Yojson.Basic.read_sequence
      (fun state x y ->
         let step = Trace.step_of_yojson (Yojson.Basic.read_json x y) in
      Replay.do_step (Model.signatures env) state step)
      (Edges.empty ~with_connected_components:true) lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = try Yojson.Basic.read_object_end lex_buf
    with Yojson.End_of_object -> () in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = close_in desc in
 ()

let main () =
  let () =
    Arg.parse
      options
      (fun f -> if !file = "" then file := f else
          let () = Format.eprintf "Deals only with 1 file" in exit 2)
      usage in
  if!file = "" then
    Arg.usage options usage
  else replay !file

let () = main ()
