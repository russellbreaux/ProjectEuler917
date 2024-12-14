val f : int -> int
val s : int -> int
val generate_sequence :
  int -> (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
val generate_matrix :
  int -> (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array2.t
val matrix_elem : int -> int -> (int, 'a, 'b) Bigarray.Array1.t -> int
val binmin : 'a -> 'a -> 'a
val cheapest_path : int -> int
val cheapest_path_precompute : int -> int
val string_of_array : int array -> string
val string_of_array1 : (int, 'a, 'b) Bigarray.Array1.t -> string
val print_array1 : (int, 'a, 'b) Bigarray.Array1.t -> unit
val print_array2 :
  (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array2.t -> unit