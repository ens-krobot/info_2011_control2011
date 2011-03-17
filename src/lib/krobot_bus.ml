(*
 * krobot_bus.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

type t = OBus_bus.t

external of_bus : OBus_bus.t -> t = "%identity"
external to_bus : t -> OBus_bus.t = "%identity"

let bus = lazy(OBus_bus.of_addresses [OBus_address.make "unix" [("abstract", "krobot")]])

let get () = Lazy.force bus
