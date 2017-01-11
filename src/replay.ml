(* ReConKa
   Copyright 2017 Harvard Medical School

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 3 as
   published by the Free Software Foundation. *)

type state = {
  graph : Edges.t;
  time : float;
  event : int;
  connected_components : Mods.IntSet.t Mods.IntMap.t;
}

let init_state = {
  graph = Edges.empty ~with_connected_components:true;
  time = 0.;
  event = 0;
  connected_components = Mods.IntMap.empty;
}

let do_negative_part ((a,_),s) graph =
  match Edges.link_destination a s graph with
  | None -> Edges.remove_free a s graph
  | Some ((a',_),s') ->
    let graph',cc_change = Edges.remove_link a s a' s' graph in
    graph'

let do_action sigs graph = function
  | Instantiation.Create ((id,ty),_graphs) ->
    snd @@ Edges.add_agent ~id sigs ty graph
  | Instantiation.Mod_internal (((a,_),s),i) ->
    Edges.add_internal a s i graph
  | Instantiation.Bind ((a1,s1 as x1),(a2,s2 as x2))
  | Instantiation.Bind_to ((a1,s1 as x1),(a2,s2 as x2)) ->
    let graph',cc_change =
      Edges.add_link a1 s1 a2 s2
        (do_negative_part x2 (do_negative_part x1 graph)) in
    graph'
  | Instantiation.Free ((a,_),s as x) ->
    Edges.add_free a s (do_negative_part x graph)
  | Instantiation.Remove (id,ty as a) ->
    Edges.remove_agent id
      (Tools.recti (fun st s -> do_negative_part (a,s) st)
         graph (Signature.arity sigs ty))

let do_step sigs state = function
  | Trace.Subs _ -> state
  | Trace.Event (_,event,info) ->
    {
      graph =
        List.fold_left
          (fun graph ((id,_),s) -> Edges.add_free id s graph)
          (List.fold_left
             (do_action sigs) state.graph
             event.Instantiation.actions)
          event.Instantiation.side_effects_dst;
      time = info.Trace.Simulation_info.story_time;
      event = info.Trace.Simulation_info.story_event;
      connected_components = state.connected_components;
    }
  | Trace.Init actions ->
    {
      graph = List.fold_left (do_action sigs) state.graph actions;
      time = state.time; event = state.event;
      connected_components = state.connected_components;
    }
  | Trace.Obs (_,_,info) ->
    {
      graph = state.graph;
      time = info.Trace.Simulation_info.story_time;
      event = info.Trace.Simulation_info.story_event;
      connected_components = state.connected_components;
    }
  | Trace.Dummy _ -> state
