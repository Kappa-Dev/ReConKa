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
  let progress = Progress_report.create 80 '#' in
  try
    let _,(plot,final) =
      Trace.fold_trace_file
        (fun env (plot,state) step ->
           let state',dist = Replay.do_step (Model.signatures env) state step in
           let () = Output.maybe_plot plot state' in
           let () = match dist.Replay.unary_distances with
             | None -> ()
             | Some (rule,length) ->
               Output.new_distance
                 (Model.get_rule env rule).Primitives.syntactic_rule
                 state'.Replay.time length in
           let () =
             Progress_report.tick
               ~efficiency:false
               state'.Replay.time 0. state'.Replay.event 0. progress in
           plot,state')
        (fun env ->
           (let rule_names =
             let size = Model.nb_syntactic_rules env + 1 in
             Array.init
               size
               (Format.asprintf "%a" (Model.print_ast_rule ~env)) in
           Output.open_plot
             ~period:!period ~distances_in_json:(not !raw_distances)
             rule_names !out_file,
           Replay.init_state ~with_connected_components:true))
        fname in
    let () = Progress_report.complete_progress_bar
        final.Replay.time final.Replay.event progress in
    Output.close_plot plot
  with
  | ExceptionDefn.Internal_Error er
  | ExceptionDefn.Malformed_Decl er ->
    let () = Pp.error Format.pp_print_string er in
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
