(* ReConKa
   Copyright 2017 Harvard Medical School

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 3 as
   published by the Free Software Foundation. *)

let do_negative_part ((a,_),s) state =
  match Edges.link_destination a s state with
  | None -> Edges.remove_free a s state
  | Some ((a',_),s') ->
    let state',cc_change = Edges.remove_link a s a' s' state in
    state'

let do_action sigs state = function
  | Instantiation.Create ((id,ty),_states) ->
    snd @@ Edges.add_agent ~id sigs ty state
  | Instantiation.Mod_internal (((a,_),s),i) ->
    Edges.add_internal a s i (snd @@ Edges.remove_internal a s state)
  | Instantiation.Bind ((a1,s1 as x1),(a2,s2 as x2))
  | Instantiation.Bind_to ((a1,s1 as x1),(a2,s2 as x2)) ->
    let state',cc_change =
      Edges.add_link a1 s1 a2 s2
        (do_negative_part x2 (do_negative_part x1 state)) in
    state'
  | Instantiation.Free ((a,_),s as x) ->
    Edges.add_free a s (do_negative_part x state)
  | Instantiation.Remove (id,ty as a) ->
    Edges.remove_agent id
      (Tools.recti (fun st s -> do_negative_part (a,s) st)
         state (Signature.arity sigs ty))

let do_step sigs state = function
  | Trace.Subs _ -> state
  | Trace.Event (_,(_,(actions,_side_src,_side_dst)),_info) ->
     List.fold_left (do_action sigs) state actions
  | Trace.Init actions -> List.fold_left (do_action sigs) state actions
  | Trace.Obs _ -> state
  | Trace.Dummy _ -> state
