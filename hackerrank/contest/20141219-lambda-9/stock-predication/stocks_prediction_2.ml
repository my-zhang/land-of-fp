
let read_int () = Scanf.scanf " %d" (fun x -> x) 

let read_array n =
  let a = Array.init n (fun _ -> 0) in
    for i = 0 to n - 1 do
      let x = Scanf.scanf " %d" (fun x -> x) in
        a.(i) <- x;
    done;
    a

type interval_tree = 
  | Node of int * int * int * int * interval_tree * interval_tree 
  | Leaf of int * int * int

let mid l r = (l + r) / 2 

let rec build_tree l r = 
  if l <> r then 
    Node (l, r, min_int, max_int, (build_tree l (mid l r)), 
                     (build_tree ((mid l r) + 1) r)) 
  else
    Leaf (l, r, 0) 

let rec insert_val tree idx v = 
  match tree with 
    | Node (l, r, max_v, min_v, left, right) -> 
        if idx <= (mid l r) then 
          Node (l, r, (max v max_v), (min v min_v), 
                (insert_val left idx v), right) 
        else 
          Node (l, r, (max v max_v), (min v min_v),
                left, (insert_val right idx v)) 

    | Leaf (l, r, u) -> Leaf (l, r, v) 

let rec query_range tree nums f pivot v = 
  match tree with 
    | Node (l, r, max_v, min_v, left, right) -> 
        if (f max_v min_v v) = v then 
          Some (l, r) 
        else 
          let mid = (l + r) / 2 in 
            if pivot <= mid then 
              let Some (a, b) = query_range left nums f pivot v in 
                if b = mid && (f nums.(mid+1) nums.(mid+1) v) = v then (
                  let q = query_range right nums f (mid + 1) v in 
                    match q with 
                      | None -> Some (a, b) 
                      | Some (c, d) -> Some (a, d) 
                )
                else
                  Some (a, b) 
            else 
              let Some (a, b) = query_range right nums f pivot v in 
                if a = (mid + 1) && (f nums.(mid) nums.(mid) v) = v then 
                  let q = query_range left nums f mid v in 
                    match q with 
                      | None -> Some (a, b) 
                      | Some (c, d) -> Some (c, b) 
                else
                  Some (a, b) 
        
    | Leaf (l, r, u) -> 
        if (f u u v) = v then 
          Some (l, r) 
        else None 

let init_tree tree nums = 
  let rec insert_init_vals tree i nums = 
    match nums with 
      | [] -> tree 
      | h::rest -> 
          let t = insert_val tree i h in 
            insert_init_vals t (i+1) rest
  in 
    insert_init_vals tree 0 nums

let main = 
  let n = Scanf.scanf " %d" (fun x -> x) in 
  let nums = read_array n in 
  let tree = init_tree (build_tree 0 (n-1)) (Array.to_list nums) in 
  let t = read_int () in 
    for i = 1 to t do 
      let idx, m = Scanf.scanf " %d %d" (fun x y -> x, y) in
      let v = nums.(idx) in 
      let fmin _ u v = (min u v) in 
      let fmax u _ v = (max u v) in 
      let Some (a, b) = query_range tree nums fmin idx v in 
      let Some (c, d) = query_range tree nums fmax idx (v + m) in 
        (* Printf.printf "DM [%d %d] [%d %d] [%d %d]\n" idx m a b c d; *) 
        Printf.printf "%d\n" ((min b d) - (max a c) + 1) 
    done 

