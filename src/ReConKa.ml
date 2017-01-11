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
  let progress = Progress_report.create 80 '#' in
  let final = Yojson.Basic.read_sequence
      (fun state x y ->
         let step = Trace.step_of_yojson (Yojson.Basic.read_json x y) in
         let state' = Replay.do_step (Model.signatures env) state step in
         let () =
           Progress_report.tick
             state'.Replay.time 0. state'.Replay.event 0. progress in
         state')
      Replay.init_state lex_st lex_buf in
  let () = Progress_report.complete_progress_bar
      final.Replay.time final.Replay.event progress in
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
  else
    try
      replay !file
    with
    | ExceptionDefn.Internal_Error er
    | ExceptionDefn.Malformed_Decl er ->
      let () = Pp.error Format.pp_print_string er in
      exit 2

let () = main ()
