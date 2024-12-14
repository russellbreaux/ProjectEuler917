(* Function library for Project Euler problem number 917 *)

(** Initial value of sequence*)
let s1 = 102022661

(** Compute next element of sequence given current element *)
let f =
    let k = 998388889 in
    let modk x = x mod k in
    let square x = x * x in
    Fun.compose modk square

(** Compute nth term of sequence *)
let s n =
    let rec aux acc i =
        if i = 1 then acc
        else aux (f acc) (i - 1)
    in aux s1 n

(** Generate 1d array of first n sequence elements *)
let generate_sequence n =
    let arr = Bigarray.Array1.create Bigarray.int Bigarray.c_layout n
    in let rec aux i =
        if i = 0 then begin
            arr.{0} <- s1;
            aux (i + 1)
        end
        else if i < n then begin
            arr.{i} <- (f arr.{i - 1});
            aux (i + 1)
        end
        else arr
    in aux 0

(** Generate the ith row of the n x n matrix *)
let generate_row i n sequence =
  Array.init n (fun j -> sequence.{2 * i} + sequence.{2 * j + 1})

(** Generate 2d array representation of matrix M where M_ij = s_2i-1 + s_2j *)
let generate_matrix dim =
  let open Bigarray in
  let seq = generate_sequence (2 * dim) in
  Array2.init int c_layout dim dim (fun i j -> seq.{2 * i} + seq.{2 * j + 1})

(** Compute element i, j of M given sequence array *)
let matrix_elem i j sequence = sequence.{2 * i} + sequence.{2 * j + 1}

(** Binary minimum *)
let binmin x y = if y < x then y else x

(** First attempt at least cost path algo given matrix size n x n *)
let cheapest_path n =
  let open Array in
  let sequence = generate_sequence (2 * n) in
  let path_costs = init n (fun j -> match j with | 0 -> matrix_elem 0 0 sequence | _ -> 0) in
  mapi_inplace (fun j x -> if j = 0 then x else path_costs.(j - 1) + matrix_elem 0 j sequence) path_costs;
  let rec arriterate i j =
    if i < n then 
      let rec iterate_on_row j =
        if j = 0 then begin
          path_costs.(0) <- path_costs.(0) + matrix_elem i 0 sequence;
          iterate_on_row (j + 1)
        end
        else if j < n then begin
          path_costs.(j) <- (binmin path_costs.(j - 1) path_costs.(j)) + matrix_elem i j sequence;
          iterate_on_row (j + 1)
        end
        else arriterate (i + 1) 0
        in iterate_on_row j
    else path_costs.(n-1)
  in arriterate 1 0

(** Second attempt at least cost path algo. Pre-computes each matrix row before computing path costs. *)
let cheapest_path_precompute n =
  let open Array in
  let sequence = generate_sequence (2 * n) in
  let path_costs = init n (fun j -> match j with | 0 -> matrix_elem 0 0 sequence | _ -> 0) in
  mapi_inplace (fun j x -> if j = 0 then x else path_costs.(j - 1) + matrix_elem 0 j sequence) path_costs;
  let rec arriterate i j =
    if i < n then 
      let current_row = generate_row i n sequence in
      let rec iterate_on_row j =
        if j = 0 then begin
          path_costs.(0) <- path_costs.(0) + current_row.(0);
          iterate_on_row (j + 1)
        end
        else if j < n then begin
          path_costs.(j) <- (binmin path_costs.(j - 1) path_costs.(j)) + current_row.(j);
          iterate_on_row (j + 1)
        end
        else arriterate (i + 1) 0
        in iterate_on_row j
    else path_costs.(n-1)
  in arriterate 1 0


(* Printing *)


(** Convert Array to string for printing *)
let string_of_array a =
  let rec aux acc i =
    if i = 0 then aux ("[| " ^ (string_of_int a.(i))) (i + 1)
    else if i = Array.length a then (acc ^ " |]")
    else aux (acc ^ "; " ^ string_of_int a.(i)) (i + 1)
  in aux "" 0

(** Convert Bigarray.Array1 to string for printing *)
let string_of_array1 a =
    let rec aux acc i =
    if i = 0 then aux ("[| " ^ (string_of_int a.{i})) (i + 1)
    else if i = Bigarray.Array1.dim a then (acc ^ " |]")
    else aux (acc ^ "; " ^ string_of_int a.{i}) (i + 1)
    in aux "" 0

(** Print array1 *)
let print_array1 a = a |> string_of_array1 |> print_endline

(** Pretty print 2d array *)
let print_array2 (arr : (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array2.t) =
  let open Bigarray in
  let open Format in
  let rows = Array2.dim1 arr in
  let cols = Array2.dim2 arr in
  let max_width =
    let max_len = ref 0 in
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        max_len := max !max_len (String.length (string_of_int (Array2.get arr i j)))
      done
    done;
    !max_len
  in
  printf "@[<v>";
  for i = 0 to rows - 1 do
    printf "@[<h>[|";
    for j = 0 to cols - 1 do
      printf "%*d " max_width (Array2.get arr i j)
    done;
    printf "|]@]@,";
  done;
  printf "@]@.";
