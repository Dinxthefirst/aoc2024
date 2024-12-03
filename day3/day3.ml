let digits = {|[0-9][0-9]?[0-9]?|}
let mul_string = Printf.sprintf "mul(%s,%s)" digits digits
let do_string = "do()"
let dont_string = "don't()"

let combined_pattern =
  Printf.sprintf "%s\\|%s\\|%s" mul_string do_string dont_string
;;

let parse_muls str_list =
  let rec aux acc enabled = function
    | [] -> acc
    | [ s ] when s = do_string || s = dont_string -> acc
    | [ s ] -> if enabled then s :: acc else acc
    | s :: tl when s = do_string -> aux acc true tl
    | s :: tl when s = dont_string -> aux acc false tl
    | s :: tl ->
      if enabled then aux (s :: acc) enabled tl else aux acc enabled tl
  in
  aux [] true str_list
;;

let parse string =
  let splits = Str.full_split (Str.regexp combined_pattern) string in
  let delims =
    List.fold_left
      (fun acc str ->
        match str with Str.Delim s -> s :: acc | Str.Text _ -> acc)
      []
      splits
  in
  let mul_list = parse_muls (List.rev delims) in
  let number_list =
    mul_list
    |> List.map (String.split_on_char ')')
    |> List.map List.hd
    |> List.map (fun x ->
      let strs = String.split_on_char '(' x in
      List.hd (List.rev strs))
    |> List.map (String.split_on_char ',')
    |> List.map (List.map int_of_string)
  in
  number_list
;;

let multiply lst = List.fold_left (fun acc x -> x * acc) 1 lst

let () =
  let contents = In_channel.with_open_bin "input.txt" In_channel.input_all in
  let mul_lists = parse contents in
  let result =
    List.fold_left (fun acc mul_list -> acc + multiply mul_list) 0 mul_lists
  in
  Printf.printf "%d\n" result
;;
