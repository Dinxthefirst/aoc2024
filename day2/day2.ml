let split_on_spaces line =
  line
  |> String.split_on_char ' '
  |> List.filter (fun x -> x <> "")
  |> List.map int_of_string
;;

let input f =
  let contents = In_channel.with_open_bin f In_channel.input_all in
  let lines =
    contents |> String.split_on_char '\n' |> List.filter (fun x -> x <> "")
  in
  List.map split_on_spaces lines
;;

let inc x y =
  let diff = y - x in
  diff > 0 && diff < 4
;;

let dec x y =
  let diff = x - y in
  diff > 0 && diff < 4
;;

let is_safe numbers =
  let rec aux f = function
    | [] | [ _ ] -> true
    | x :: (y :: _ as tl) -> f x y && aux f tl
  in
  aux inc numbers || aux dec numbers
;;

let remove_at idx lst =
  let rec aux i acc = function
    | [] -> acc
    | x :: xs ->
      if i = idx then aux (i + 1) acc xs else aux (i + 1) (x :: acc) xs
  in
  List.rev (aux 0 [] lst)
;;

let check_safety (numbers : int list) =
  let len = List.length numbers in
  let rec aux i =
    if i >= len
    then false
    else if is_safe (remove_at i numbers)
    then true
    else aux (i + 1)
  in
  is_safe numbers || aux 0
;;

let () =
  let numbers = input "input.txt" in
  let safe_lists = List.map check_safety numbers in
  let num_of_safe_lists =
    List.fold_left (fun acc x -> if x then acc + 1 else acc) 0 safe_lists
  in
  Printf.printf "%d" num_of_safe_lists
;;
