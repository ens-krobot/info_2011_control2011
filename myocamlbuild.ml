(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
         | Before_options ->
             Options.make_links := false

         | After_rules ->
             rule "D-Bus interface generation: .obus -> .ml, .mli"
               ~dep:"%.obus" ~prods:["%.ml"; "%.mli"]
               (fun env _ -> Cmd(S[A"obus-gen-interface"; A"-o"; A(env "%"); A(env "%.obus")]));

             rule ".glade -> .ml" ~dep:"%.glade" ~prod:"%.ml"
               (fun env _ ->
                  Cmd(S[A"lablgladecc2"; A"-embed"; A(env "%.glade"); Sh">"; A(env "%.ml")]))

         | _ ->
             ())

