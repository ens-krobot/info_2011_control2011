
open Krobot_geom

val find_path :
  ?src_orient:float * vector ->
  ?dst_orient:float * vector ->
  src:vertice ->
  dst:vertice ->
  vertice * vertice ->
  ( vertice * float ) list ->
  vertice list option
