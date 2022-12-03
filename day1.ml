let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in loop [];;

             
let score_pair pair =
   let second = String.get pair 2 in
   let score_second = match second with
     | 'X' -> 1
     | 'Y' -> 2
     | 'Z' -> 3
     | _ -> 0 in
   let score_win = match pair with 
     | "A Z" -> 0
     | "B X" -> 0
     | "C Y" -> 0
     | "A X" -> 3
     | "B Y" -> 3
     | "C Z" -> 3
     | "A Y" -> 6
     | "B Z" -> 6
     | "C X" -> 6
     | _ -> 0 in
   (score_win  + score_second );;
let score_victory pair =
   let second = String.get pair 2 in
   let score_second = match second with
     | 'X' -> 0
     | 'Y' -> 3
     | 'Z' -> 6
     | _ -> 0 in
   let score_type = match pair with 
     | "A Z" -> 2
     | "B X" -> 1
     | "C Y" -> 3
     | "A X" -> 3
     | "B Y" -> 2
     | "C Z" -> 1
     | "A Y" -> 1
     | "B Z" -> 3
     | "C X" -> 2
     | _ -> 0 in
   (score_type  + score_second );; 
    
((read_lines "day1.txt")  |> (List.map score_pair)) |> List.fold_left (fun a b -> a+b)  (0) |> print_int;
print_endline "";
((read_lines "day1.txt")  |> (List.map score_victory )) |> List.fold_left (fun a b -> a+b)  (0) |> print_int  ;
