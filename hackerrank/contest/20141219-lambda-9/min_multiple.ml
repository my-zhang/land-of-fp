#load "str.cma"

let m = 1000000007 

let primes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 
              47; 53; 59; 61; 67; 71; 73; 79; 83; 89; 97]

let ( ** ) x y =
  let rec loop prod a b =
    match b with
      | 0 -> 
          prod
      | _ when b land 1 <> 0 -> 
          loop (prod * a mod m) (a * a mod m) (b / 2) 
      | _ -> loop prod (a * a mod m) (b / 2)
  in
    loop 1 x y

let mul_mod x y = 
  (x * y) mod m

let vec_lcm v u = 
  List.map2 max v u 

let vec_mul v u = 
  List.map2 (fun x y -> (x + y)) v u 

let vec_of_int n = 
  let factor p = 
    let rec loop i p cnt = 
      if (i mod p) <> 0 then 
        cnt 
      else 
        loop (i / p) p (cnt + 1) 
    in 
      loop n p 0 
  in
    List.map factor primes 

let int_of_vec v = 
  List.fold_left mul_mod 1 (List.map2 (fun x y -> x ** y) primes v) 

let empty_vec = vec_of_int 1 

type interval_tree = 
  | Node of int * int * int list * interval_tree * interval_tree 
  | Leaf of int * int * int list

let mid l r = (l + r) / 2 

let rec build_tree l r = 
  if l <> r then 
    Node (l, r, empty_vec, (build_tree l (mid l r)), 
                     (build_tree ((mid l r) + 1) r)) 
  else
    Leaf (l, r, empty_vec) 

let rec insert_val tree idx u = 
  match tree with 
    | Node (l, r, v, left, right) -> 
        if idx <= (mid l r) then 
          Node (l, r, (vec_lcm v u), 
                (insert_val left idx u), right) 
        else 
          Node (l, r, (vec_lcm v u), 
                left, (insert_val right idx u)) 

    | Leaf (l, r, v) -> Leaf (l, r, u) 

let init_tree tree nums = 
  let rec insert_init_vals tree i nums = 
    match nums with 
      | [] -> tree 
      | h::rest -> 
          let t = insert_val tree i (vec_of_int h) in 
            insert_init_vals t (i+1) rest  
  in 
    insert_init_vals tree 0 nums 

(* 
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
 *)

let rec query_segs tree i j = 
  match tree with 
    | Node (l, r, v, left, right) -> 
        if i = l && l = r then 
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
    List.fold_left vec_lcm empty_vec segs

let main = 
  let n = read_int () in 
  let nums = List.map int_of_string 
             (Str.split (Str.regexp " ") (read_line ())) in 
  let tree = init_tree (build_tree 0 (n-1)) nums in 
  let t = read_int () in 
  let rec do_queries tree t = 
    if t > 0 then 
      let q = Str.split (Str.regexp " ") (read_line ()) in 
        match q with 
          | "Q"::l_str::r_str::rest -> 
              Printf.printf "%d\n" 
                (int_of_vec (query tree (int_of_string l_str) 
                              (int_of_string r_str)));
              do_queries tree (t-1) 
              
          | "U"::i_str::v_str::rest -> 
              let i = int_of_string i_str in 
              let u = query tree i i in
              let v = vec_mul u (vec_of_int (int_of_string v_str)) in 
                do_queries (insert_val tree i v) (t-1)
    else () 
  in
    do_queries tree t

