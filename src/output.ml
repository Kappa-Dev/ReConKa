(* This file is part of ReConKa.
   Copyright 2017 Harvard Medical School

   ReConKa is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 3 as
   published by the Free Software Foundation. *)

let outputDirName = ref ""

let mk_dir_r d =
  let rec aux d =
    let par = Filename.dirname d in
    let () = if not (Sys.file_exists par) then aux par in
    Unix.mkdir d 0o775 in
  Unix.handle_unix_error aux d

let set_dir s =
  let () = try
      if not (Sys.is_directory s)
      then (Format.eprintf "'%s' is not a directory@." s ; exit 1)
    with Sys_error _ -> mk_dir_r s in
  outputDirName := s

let path f =
  if Filename.is_implicit f && Filename.dirname f = Filename.current_dir_name
  then Filename.concat !outputDirName f
  else f

let open_out f =
  open_out (path f)

let jsonDistancesDescr = ref false
let distances = ref (None : (string array * (float * int) list array) option)
let distancesFileName = ref "distances"

let set_distances f = distancesFileName := f
let get_distances () = !distancesFileName
let with_channel str f =
  if str <> ""  then
    let desc = open_out str in
    let () = f desc in
    close_out desc

let with_formatter str f =
  with_channel
    str
    (fun desc ->
     let fr = Format.formatter_of_out_channel desc in
     let () = f fr in
     Format.pp_print_flush fr ())

let with_unary_distances f =
  let str = !distancesFileName^".json" in
  with_formatter str f

let new_distance rule time length =
  match !distances with
  | None -> ()
  | Some (_,tab) -> tab.(rule) <- (time,length)::tab.(rule)

let print_json_of_unary_distances f unary_distances rules =
  (*unary_distances: (float, int) option list array
    one_big_list: (int, float, int) list*)
  let one_big_list =
    Tools.array_fold_lefti
      (fun i l ls ->
         List.fold_left (fun acc (t,d) -> (i,t,d)::acc) l ls)
      [] unary_distances in
  Format.fprintf
    f "[%a]@."
    (Pp.list
       Pp.comma
       (fun f (id,time,distance) ->
          Format.fprintf
            f "@[{ \"rule\" : \"%s\", @ \"time\" : %e, @ \"distance\" : %d }@]"
            rules.(id) time distance)) (List.rev one_big_list)

let json_of_unary_distances unary_distances rules =
  with_unary_distances
    (fun f -> print_json_of_unary_distances f unary_distances rules)

let print_out_of_unary_distances f distances rule_name =
  let () = Format.fprintf f "Rule %s: " rule_name in
  let () = Format.fprintf f "@[<h>%s @]@." "time distance" in
  Format.fprintf f "@[%a@]"
    (Pp.list Pp.space (fun f (time,distance) ->
         Format.fprintf f "@[ %e @ %d @]@."
           time distance)) distances

let out_of_unary_distances unary_distances rules =
  Array.iteri (fun id distances_list ->
      match distances_list with
      | [] -> ()
      | ls ->
        (*create the file *)
        let filename = get_distances () in
        let filename_string = filename^(string_of_int id)^".out" in
        let d = open_out filename_string in
        let f = Format.formatter_of_out_channel d in
        (*print data*)
        let () = print_out_of_unary_distances f ls rules.(id) in
        (*close the file*)
        close_out d) unary_distances

let output_unary_distances in_json distances_data distances_rules =
  if in_json
  then json_of_unary_distances distances_data distances_rules
  else out_of_unary_distances distances_data distances_rules

type plot = { period : int; chan : out_channel; form : Format.formatter }

let open_plot ~period ~distances_in_json rule_names file_name =
  let () = jsonDistancesDescr := distances_in_json in
  let () =
    distances := Some (rule_names, Array.make (Array.length rule_names) []) in
  let chan = if file_name <> "" then open_out file_name else stdout in
  let form = Format.formatter_of_out_channel chan in
  { period; chan; form }

let size_landscape ccs =
  let out = Mods.DynArray.create 0 0 in
  let () =
    Mods.IntMap.iter
      (fun _ s -> let ss = Mods.IntSet.size s in
      Mods.DynArray.set out ss (succ (Mods.DynArray.get out ss)))
      ccs in
  out

let maybe_plot p state =
  if state.Replay.event mod p.period = 0 && state.Replay.event > 0 then
    Format.fprintf p.form "@[<h>%f,%a@]@." state.Replay.time
      (Mods.DynArray.print Pp.comma (fun _ -> Format.pp_print_int))
      (size_landscape state.Replay.connected_components)

let close_plot p =
  let () =
    match !distances with
    | None -> ()
    | Some (rules,data) ->
      output_unary_distances !jsonDistancesDescr data rules in
  let () = Format.pp_flush_formatter p.form in
  let () = close_out p.chan in
  ()
