module IntMap = Map.Make (Int)

let parse filename =
  let contents = In_channel.with_open_bin filename In_channel.input_all in
  let lines = String.split_on_char '\n' contents in
  let rec split acc = function
    | [] -> assert false
    | "" :: tl -> List.rev acc, List.rev tl
    | hd :: tl -> split (hd :: acc) tl
  in
  let ordering_strings, update_strings = split [] lines in
  let updates =
    update_strings
    |> List.map (fun s -> String.split_on_char ',' s)
    |> List.map (List.map int_of_string)
  in
  let ordering_rules =
    List.fold_left
      (fun acc s ->
        let parts = String.split_on_char '|' s in
        let key = List.nth parts 1 |> int_of_string in
        let value = List.hd parts |> int_of_string in
        let new_values =
          match IntMap.find_opt key acc with
          | None -> [ value ]
          | Some v -> value :: v
        in
        IntMap.add key new_values acc)
      IntMap.empty
      ordering_strings
  in
  ordering_rules, updates
;;

let find_middle_value l = List.nth l (List.length l / 2)

let correct_updates
  (ordering_rules : int list IntMap.t)
  (updates : int list list)
  : int
  =
  let rec is_correct_update acc = function
    | [] -> true
    | hd :: tl ->
      let values =
        IntMap.find_opt hd ordering_rules |> Option.value ~default:[]
      in
      if List.for_all (fun v -> List.mem v values) acc
      then is_correct_update (hd :: acc) tl
      else false
  in
  let middle_values =
    List.fold_left
      (fun acc update ->
        if is_correct_update [] update
        then find_middle_value update :: acc
        else acc)
      []
      updates
  in
  List.fold_left ( + ) 0 middle_values
;;

let () =
  let ordering_rules, updates = parse "input.txt" in
  (* List.iter
     (fun (k, v) ->
     Printf.printf "%d -> [" k;
      List.iter (fun i -> Printf.printf "%d; " i) v;
      Printf.printf "]\n")
     (IntMap.bindings ordering_rules);
     List.iter
     (fun update ->
     List.iter (fun i -> Printf.printf "%d, " i) update;
     Printf.printf "\n")
     updates *)
  let result = correct_updates ordering_rules updates in
  Printf.printf "%d\n" result
;;
