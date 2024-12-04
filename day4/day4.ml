let length = 140

let wihin_bounds str (x, y) =
  x >= 0 && x < length && y >= 0 && y < String.length str / length
;;

let get_char_at str (x, y) =
  let index = (y * length) + x in
  if index >= 0 && index < String.length str
  then Some (String.get str index)
  else None
;;

let next_char str (x, y) =
  match get_char_at str (x, y) with
  | Some 'M' -> 'S'
  | Some 'S' -> 'M'
  | _ -> ' '
;;

let check_diagonal str (x, y) (dx1, dy1) (dx2, dy2) =
  let new_pos = x + dx1, y + dy1 in
  if not (wihin_bounds str new_pos)
  then false
  else (
    match next_char str new_pos with
    | ' ' -> false
    | ('M' | 'S') as next_c ->
      let new_pos' = x + dx2, y + dy2 in
      if not (wihin_bounds str new_pos')
      then false
      else (
        let new_char = get_char_at str new_pos' in
        match new_char with Some c when c = next_c -> true | _ -> false)
    | _ -> false)
;;

let search_directions str positions =
  let aux str (x, y) acc =
    let one_direction = check_diagonal str (x, y) (-1, -1) (1, 1) in
    let two_direction = check_diagonal str (x, y) (-1, 1) (1, -1) in
    if one_direction && two_direction then acc + 1 else acc
  in
  List.fold_left (fun acc pos -> aux str pos acc) 0 positions
;;

let find_a_occurences str =
  let rec aux acc pos =
    match String.index_from str pos 'A' with
    | exception Not_found -> acc
    | pos -> aux (pos :: acc) (pos + 1)
  in
  aux [] 0
;;

let () =
  let contents = In_channel.with_open_bin "input.txt" In_channel.input_all in
  let input = contents |> String.split_on_char '\n' |> String.concat "" in
  let x_positions = find_a_occurences input in
  let positions = List.map (fun x -> x mod length, x / length) x_positions in
  let result = search_directions input positions in
  Printf.printf "%d\n" result
;;
