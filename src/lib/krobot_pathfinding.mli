
open Krobot_geom

val find_path :
  src:vertice ->
  dst:vertice ->
  vertice * vertice ->
  ( vertice * float ) list ->
  vertice list option
