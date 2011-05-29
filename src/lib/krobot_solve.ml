let epsilon = 0.0000000001

let add_l ~m ~v i (n,j) =
  let a = Array.init (Array.length m.(i))
    (fun k -> m.(i).(k) +. n *. m.(j).(k)) in
  let m = Array.init (Array.length m)
    (fun k -> if k <> i then m.(k) else a) in
  let v = Array.init (Array.length v)
    (fun k -> if k <> i then v.(k) else v.(i) +. n *. v.(j)) in
  m,v

let remove_l ~m ~v i =
  let rec remove_l ~m ~v i j =
    if j >= Array.length m
    then m,v
    else
      let m,v = add_l ~m ~v j (-. (m.(j).(i) /. m.(i).(i)),i) in
      remove_l ~m ~v i (j+1) in
  remove_l ~m ~v i (i+1)

let switch_a a i j =
  Array.init (Array.length a)
    (function
      | k when k = i -> a.(j)
      | k when k = j -> a.(i)
      | k -> a.(k))

let switch_l ~m ~v i j =
  switch_a m i j,
  switch_a v i j

let reord_l ~m ~v i =
  let rec reord_l ~m ~v j =
    if j >= Array.length m
    then m,v
    else
      if abs_float (m.(j).(i)) < epsilon
      then reord_l ~m ~v (j+1)
      else switch_l ~m ~v i j
  in
  reord_l ~m ~v i

let triangle m v =
  let rec triangle m v i =
    if i >= Array.length m - 1
    then m,v
    else
      let m,v = reord_l ~m ~v i in
      if abs_float (m.(i).(i)) <= epsilon
      then triangle m v (i+1)
      else
	let m,v = remove_l ~m ~v i in
	triangle m v (i+1)
  in
  triangle m v 0

let apply_res m v i =
  let r = v.(i) /. m.(i).(i) in
  let v = Array.init (Array.length v)
    (fun k ->
      if k >= i
      then
	if k = i
	then v.(k) /. m.(i).(i)
	else v.(k)
      else v.(k) -. m.(k).(i) *. r) in
  let m = Array.init (Array.length m)
    (fun k ->
      if k = i
      then Array.init (Array.length m.(k))
	(fun l -> if l = i then 1. else 0.)
      else Array.init (Array.length m.(k))
      (fun l ->
	if l = i
	then 0.
	else m.(k).(l) )) in
  m,v

let solve m v =
  let m,v = triangle m v in
  let rec solve m v i =
    if i < 0
    then m,v
    else
      let m,v = apply_res m v i in
      solve m v (i-1)
  in
  snd (solve m v (Array.length m - 1))

let mult m v =
  Array.init (Array.length m)
    (fun k -> Array.fold_left (+.) 0.
      (Array.mapi (fun i n -> v.(i) *. n) m.(k)))
