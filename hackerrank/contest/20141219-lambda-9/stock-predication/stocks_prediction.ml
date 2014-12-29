#load "str.cma" 

type interval_tree = 
  | Node of int * int * int * int * interval_tree * interval_tree 
  | Leaf of int * int * int * int 

let mid l r = (l + r) / 2 

let rec build_tree l r = 
  if l <> r then 
    Node (l, r, 0, max_int, (build_tree l (mid l r)), 
                     (build_tree ((mid l r) + 1) r)) 
  else
    Leaf (l, r, 0, max_int) 

let rec insert_val tree idx v = 
  match tree with 
    | Node (l, r, max_v, min_v, left, right) -> 
        if idx <= (mid l r) then 
          Node (l, r, (max v max_v), (min v min_v), 
                (insert_val left idx v), right) 
        else 
          Node (l, r, (max v max_v), (min v min_v),
                left, (insert_val right idx v)) 

    | Leaf (l, r, max_v, min_v) -> Leaf (l, r, v, v) 

let rec query_segs tree i j = 
  match tree with 
    | Node (l, r, v, u, left, right) -> 
        if i = l && l = r then 
          [(v, u)]
        else if j <= (mid l r) then 
          (query_segs left i j) 
        else if i > (mid l r) then 
          (query_segs right i j)
        else
          (query_segs left i (mid l r)) @ 
            (query_segs right ((mid l r) + 1) j) 

    | Leaf (l, r, v, u) -> [(v, u)] 

let query tree i j = 
  let f a b = 
    let (max_a, min_a) = a in 
    let (max_b, min_b) = b in 
      ((max max_a max_b), (min min_a min_b)) 
  in
  let segs = query_segs tree i j in
    List.fold_left f (0, max_int) segs

let init_tree tree nums = 
  let rec insert_init_vals tree i nums = 
    match nums with 
      | [] -> tree 
      | h::rest -> 
          let t = insert_val tree i h in 
            insert_init_vals t (i+1) rest
  in 
    insert_init_vals tree 0 nums

let rec left_binsearch tree l r idx v mv = 
  if l > r then l 
  else 
    let m = (l + r) / 2 in 
    let (max_v, min_v) = query tree m idx in 
      if v > min_v || max_v > mv then 
        left_binsearch tree (m+1) r idx v mv 
      else 
        left_binsearch tree l (m-1) idx v mv 

let rec right_binsearch tree l r idx v mv = 
  if l > r then r
  else 
    let m = (l + r) / 2 in 
    let (max_v, min_v) = query tree idx m in 
      (* Printf.printf "binsearch [%d %d] %d [%d %d]\n" 
       *  idx m v max_v min_v; *) 
      if v > min_v || max_v > mv then 
        right_binsearch tree l (m-1) idx v mv 
      else 
        right_binsearch tree (m+1) r idx v mv 

let do_queries tree n t = 
  let g idx m = 
    let (v, _) = query tree idx idx in 
    let l = left_binsearch tree 0 idx idx v (v + m) in 
    let r = right_binsearch tree idx (n-1) idx v (v + m) in 
      (* Printf.printf "l: %d r:%d\n" l r; *) 
      r - l + 1
  in 
  let rec f i = 
    if i < t then
      let q = List.map int_of_string 
             (Str.split (Str.regexp " ") (read_line ())) in 
        match q with 
          | idx::m::rest -> 
            (* Printf.printf "query %d %d\n" idx m; *) 
            Printf.printf "%d\n%!" (g idx m); 
            f (i+1)  
          | _ -> () 
    else () 
  in 
    f 0 

let main = 
  let n = read_int () in 
  let nums = List.map int_of_string 
             (Str.split (Str.regexp " ") (read_line ())) in 
  let tree = init_tree (build_tree 0 (n-1)) nums in 
  let t = read_int() in 
    do_queries tree n t 

