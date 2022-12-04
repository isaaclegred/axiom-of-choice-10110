let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in loop [];;

let rec find_dup first second =
  match String.contains first (String.get second 0) with
  | true -> String.get second 0
  | false -> find_dup first (String.sub second 1 (String.length second-1))
let eval_priorty duped = 
  match duped < 'a' with
  (* Uppercase *)
  | true -> (Char.code duped) -  38
  (* Lowercase *)
  | false -> (Char.code duped) - 96;;

let find_priorty s =
  let split_point = (String.length s)/2 in
  let first = String.sub s 0 split_point in
  let second = String.sub s split_point split_point in
  let duped = find_dup first second in
  eval_priorty duped 

let rec find_shared elf1 elf2 elf3 =
  let h = String.get elf1 0 in
  let t = String.sub elf1 1 (String.length elf1 - 1) in
  match String.contains elf2 h with
  | true -> (
      match String.contains elf3 h with         
      | true -> h
      | false ->  find_shared t elf2 elf3
    )
  | false -> find_shared t elf2 elf3




    
let rec part_two lines acc=
  match lines with
  | [] -> acc 
  | elf1::elf2::elf3::t -> part_two t (acc + ( eval_priorty (find_shared elf1 elf2 elf3) ))
  | _ -> -1 ;;


read_lines "day2.txt" |> List.map find_priorty |> List.fold_left (fun a b -> a + b) 0 |> print_int;
print_endline  "";
part_two (read_lines "day2.txt") 0 |> print_int
  
(* print_int (find_priorty "vJrwpWtwJgWrhcsFMMfFFhFp") *)

