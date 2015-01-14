#load "str.cma" 

type interval_tree = 
  | Node of int * int * int * interval_tree * interval_tree 
  | Leaf of int * int * int 

let accum_func  = max 

let mid l r = (l + r) / 2 

let rec build_tree l r = 
  if l <> r then 
    Node (l, r, 0, (build_tree l (mid l r)), 
                     (build_tree ((mid l r) + 1) r)) 
  else
    Leaf (l, r, 0) 

let rec insert_val tree idx value = 
  match tree with 
    | Node (l, r, v, left, right) -> 
        if idx <= (mid l r) then 
          Node (l, r, (accum_func v value), 
                (insert_val left idx value), right) 
        else 
          Node (l, r, (accum_func v value), 
                left, (insert_val right idx value)) 

    | Leaf (l, r, v) -> Leaf (l, r, value) 

let init_tree tree nums = 
  let rec insert_init_vals tree i nums = 
    match nums with 
      | [] -> tree 
      | h::rest -> 
          let t = insert_val tree i h in 
            insert_init_vals t (i+1) rest  
  in 
    insert_init_vals tree 0 nums 

let rec preorder_print (tree : interval_tree) = 
  match tree with 
    | Node (l, r, v, left, right) -> 
        Printf.printf "[%d %d] %d \n" l r v;
        preorder_print left; 
        preorder_print right; 
        ()
    | Leaf (l, r, v) -> 
        Printf.printf "[%d %d] %d \n" l r v;
        () 

let rec query_segs tree i j = 
  match tree with 
    | Node (l, r, v, left, right) -> 
        if i = l && j = r then 
          [v]
        else if j <= (mid l r) then 
          (query_segs left i j) 
        else if i > (mid l r) then 
          (query_segs right i j)
        else
          (query_segs left i (mid l r)) @ 
            (query_segs right ((mid l r) + 1) j) 

    | Leaf (l, r, v) -> [v] 

let query tree i j = 
  let segs = query_segs tree i j in
    List.fold_left accum_func 0 segs

let main = 
  let n = read_int () in 
  let nums = List.map int_of_string 
             (Str.split (Str.regexp " ") (read_line ())) in 
  let tree = init_tree (build_tree 0 (n-1)) nums in 
  let m = read_int () in 
  let rec do_queries tree m = 
    if m > 0 then 
      let q = Str.split (Str.regexp " ") (read_line ()) in 
        match q with 
          | "Q"::l_str::r_str::rest -> 
              Printf.printf "%d\n" 
                (query tree (int_of_string l_str) (int_of_string r_str));
              do_queries tree (m-1) 
              
          | "U"::i_str::v_str::rest -> 
              do_queries (insert_val tree (int_of_string i_str)
                            (int_of_string v_str)) (m-1)
    else () 
  in
    do_queries tree m 

