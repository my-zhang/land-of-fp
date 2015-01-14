
let read_int () = Scanf.scanf " %d" (fun x -> x) 

let read_array n =
  let a = Array.init n (fun _ -> 0) in
    for i = 0 to n - 1 do
      let x = Scanf.scanf " %d" (fun x -> x) in
        a.(i) <- x;
    done;
    a

type interval_tree = 
  | Node of int * int * int * interval_tree * interval_tree 
  | Leaf of int * int * int 

let mid l r = (l + r) / 2 

let rec build_tree l r = 
  if l <> r then 
    Node (l, r, max_int, (build_tree l (mid l r)), 
                     (build_tree ((mid l r) + 1) r)) 
  else
    Leaf (l, r, max_int) 

let rec insert_val tree idx value = 
  match tree with 
    | Node (l, r, v, left, right) -> 
        if idx <= (mid l r) then 
          Node (l, r, (min v value), 
                (insert_val left idx value), right) 
        else 
          Node (l, r, (min v value), 
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

let rec query tree i j = 
  match tree with 
    | Node (l, r, v, left, right) -> 
        if i = l && j = r then 
          v
        else if j <= (mid l r) then 
          (query left i j) 
        else if i > (mid l r) then 
          (query right i j)
        else
          (min (query left i (mid l r))
               (query right ((mid l r) + 1) j))

    | Leaf (l, r, v) -> v


let main = 
  let n, t = Scanf.scanf " %d %d" (fun x y -> x, y) in 
  let nums = read_array n in 
  let tree = init_tree (build_tree 0 (n-1)) (Array.to_list nums) in 
    for i = 1 to t do 
      let l, r = Scanf.scanf " %d %d" (fun x y -> x, y) in 
      let min_val = query tree l r in 
        Printf.printf "%d\n" min_val 
    done 
  
