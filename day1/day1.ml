let first list = List.hd list
let rec last = function [] -> assert false | [ x ] -> x | _ :: tl -> last tl

let split_on_spaces line =
  line |> String.split_on_char ' ' |> List.filter (fun x -> x <> "")
;;

let input f =
  let contents = In_channel.with_open_bin f In_channel.input_all in
  let lines = String.split_on_char '\n' contents in
  List.map split_on_spaces lines
;;

let split_list list =
  List.split
    (List.map (fun x -> int_of_string (first x), int_of_string (last x)) list)
;;

let similarity list elem =
  let occurences =
    List.fold_left (fun acc x -> if x = elem then acc + 1 else acc) 0 list
  in
  occurences * elem
;;

let sum_of_similarities list1 list2 =
  List.fold_left (fun acc elem -> similarity list1 elem + acc) 0 list2
;;

let () =
  let numbers = lines "input.txt" |> List.map split_on_spaces in
  let left_list, right_list = split_list numbers in
  let result = sum_of_similarities left_list right_list in
  Printf.printf "%d\n" result
;;
