(*
 * krobot_planner.ml
 * -----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Lwt_react
open Krobot_geom
open Krobot_interface_planner.Fr_krobot_Planner

type t = Krobot_bus.t

let proxy bus =
  OBus_proxy.make (OBus_peer.make (Krobot_bus.to_bus bus) "fr.krobot.Service.Planner") ["fr"; "krobot"; "Planner"]

let vertices bus =
  OBus_property.map_rw
    (List.map (fun (x, y) -> { x; y }))
    (List.map (fun { x; y } -> (x, y)))
    (OBus_property.make p_vertices (proxy bus))

let add_vertice bus { x; y } =
  OBus_method.call m_add_vertice (proxy bus) (x, y)

let simplify bus tolerance =
  OBus_method.call m_simplify (proxy bus) tolerance

let moving bus =
  OBus_property.make p_moving (proxy bus)

let go bus a b c d =
  OBus_method.call m_go (proxy bus) (a, b, c, d)

let stop bus =
  OBus_method.call m_stop (proxy bus) ()
