(* This file is part of ReConKa.
   Copyright 2017 Harvard Medical School

   ReConKa is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 3 as
   published by the Free Software Foundation. *)

let size_landscape ccs =
  let out = Mods.DynArray.create 0 0 in
  let () =
    Mods.IntMap.iter
      (fun _ s -> let ss = Mods.IntSet.size s in
      Mods.DynArray.set out ss (succ (Mods.DynArray.get out ss)))
      ccs in
  out

type plot = { period : int; chan : out_channel; form : Format.formatter }

let open_plot ~period file_name =
  let chan = if file_name <> "" then open_out file_name else stdout in
  let form = Format.formatter_of_out_channel chan in
  { period; chan; form }

let maybe_plot p state =
  if state.Replay.event mod p.period = 0 && state.Replay.event > 0 then
    Format.fprintf p.form "@[<h>%f,%a@]@." state.Replay.time
      (Mods.DynArray.print Pp.comma (fun _ -> Format.pp_print_int))
      (size_landscape state.Replay.connected_components)

let close_plot p =
  let () = Format.pp_flush_formatter p.form in
  let () = close_out p.chan in
  ()
