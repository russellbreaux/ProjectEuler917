module StdArray = Stdlib.Array

open Euler917.Sequence
open Core


let () =
let n = 
  if StdArray.length (Sys.get_argv ()) > 1 
    then int_of_string (StdArray.get (Sys.get_argv ()) 1) 
  else 10 in
let start_time = Time_float.now () in
let cost = cheapest_path n in
let end_time = Time_float.now () in

Printf.printf "\nA(%s) = " (string_of_int n);
cost |> string_of_int |> print_endline;

let elapsed = Time_float.diff end_time start_time in
Printf.printf "Time taken = %s\n\n" (Time_float.Span.to_string elapsed)