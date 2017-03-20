(* This file is part of ReConKa.
   Copyright 2017 Harvard Medical School

   ReConKa is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 3 as
   published by the Free Software Foundation. *)

let in_file = ref ""
let out_file = ref "connectivity.csv"
let period = ref 1000
let raw_distances = ref false

let usage =
  Sys.argv.(0) ^
  " replay a Kappa simulation trace and give connectivity information on it"

let options = [
  "-p",Arg.Set_int period,"Plot period";
  "-o",Arg.Set_string out_file,"output file";
  "-d",Arg.String Output.set_dir,"Output directory";
  "--csv",Arg.Set raw_distances,"output only csv files";
]

let replay fname =
  let desc = open_in fname in
  let lex_buf = Lexing.from_channel desc in
  let lex_st = Yojson.init_lexer ~fname () in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = Yojson.Basic.read_lcurl lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let ident = Yojson.Basic.read_ident lex_st lex_buf in
  let () = assert (ident = "uuid") in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = Yojson.Basic.read_colon lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let _ = Yojson.Basic.read_string lex_st lex_buf in
  let () = Yojson.Basic.read_space lex_st lex_buf in
  let () = Yojson.Basic.read_comma lex_st lex_buf in
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
  let rule_names =
    let size = Model.nb_syntactic_rules env + 1 in
    Array.init
      size
      (Format.asprintf "%a" (Model.print_ast_rule ~env)) in
  let plot = Output.open_plot
      ~period:!period ~distances_in_json:(not !raw_distances)
      rule_names !out_file in
  let progress = Progress_report.create 80 '#' in
  try
    let final = Yojson.Basic.read_sequence
        (fun state x y ->
           let step = Trace.step_of_yojson (Yojson.Basic.read_json x y) in
           let state',dist = Replay.do_step (Model.signatures env) state step in
           let () = Output.maybe_plot plot state' in
           let () = match dist with
             | None -> ()
             | Some (rule,length) ->
               Output.new_distance rule state'.Replay.time length in
           let () =
             Progress_report.tick
               state'.Replay.time 0. state'.Replay.event 0. progress in
           state')
        (Replay.init_state ~with_connected_components:true) lex_st lex_buf in
    let () = Progress_report.complete_progress_bar
        final.Replay.time final.Replay.event progress in
    let () = Yojson.Basic.read_space lex_st lex_buf in
    let () = try Yojson.Basic.read_object_end lex_buf
      with Yojson.End_of_object -> () in
    let () = Yojson.Basic.read_space lex_st lex_buf in
    let () = close_in desc in
    Output.close_plot plot
  with
  | ExceptionDefn.Internal_Error er
  | ExceptionDefn.Malformed_Decl er ->
    let () = Pp.error Format.pp_print_string er in
    let () = Output.close_plot plot in
    let () = close_in desc in
    exit 2

let main () =
  let () =
    Arg.parse
      options
      (fun f -> if !in_file = "" then in_file := f else
          let () = Format.eprintf "Deals only with 1 file" in exit 2)
      usage in
  if!in_file = "" then
    Arg.usage options usage
  else
    replay !in_file

let () = main ()
