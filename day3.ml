let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in loop [];;

let find_containment pair=
  let elts = String.split_on_char ',' pair in
  let first = List.hd elts |> String.split_on_char '-' |> List.map int_of_string in 
  let second = List.nth elts 1 |> String.split_on_char '-' |> List.map int_of_string in 
  let first_is_at_least_as_low = (List.hd first) <= (List.hd second) in
  let first_is_at_least_as_high = (List.nth first 1) >= (List.nth second 1) in
  match first_is_at_least_as_low with
  (*first is lower and first is higher, second is contained *)
  | true ->  first_is_at_least_as_high or (List.nth first 0) = (List.nth second 0)
  (*First is not lower, so second is certainly lower, on the high side, if first is lower than second it is contined
  otherwise check if first and second are the same at the high end, if so then second contains first*)
  | false -> (not first_is_at_least_as_high) or (List.nth first 1) = (List.nth second 1);;
let find_overlap pair=
  let elts = String.split_on_char ',' pair in
  let first = List.hd elts |> String.split_on_char '-' |> List.map int_of_string in 
  let second = List.nth elts 1 |> String.split_on_char '-' |> List.map int_of_string in 
  let first_goes_below = (List.hd first) <= (List.nth second 0) in
  let first_goes_above = (List.nth first 1) >= (List.nth second 0) in
  match first_goes_below with
  (*first straddles the lower bound of second?*)
  | true ->  first_goes_above 
   (*first straddles the upper bound of second?*)
  | false -> (List.hd first) <= (List.nth second 1);;

let counts = function
  | true  -> 1
  | false -> 0;;
let solve1 pair  = pair |> find_containment |> counts;;
let solve2 pair = pair |> find_overlap |> counts;;
read_lines "day3.txt" |> List.map solve1 |> List.fold_left (fun a b -> a + b) 0 |> print_int;
print_endline  "";
read_lines "day3.txt" |> List.map solve2 |> List.fold_left (fun a b -> a + b) 0 |> print_int;
print_endline  "";

print_endline (string_of_bool (find_containment "6-6,6-8"));
  
(* print_int (find_priorty "vJrwpWtwJgWrhcsFMMfFFhFp") *)

