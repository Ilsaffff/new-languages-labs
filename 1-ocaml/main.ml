let graph = [
  (1, [2; 3]);
  (2, [4; 5]);
  (3, [6]);
  (4, []);
  (5, [6]);
  (6, [])
]

let neighbors graph node =
  try List.assoc node graph
  with Not_found -> []

let rec find_shortest_path graph visited current target =
  if current = target then Some [current]
  else
    let visited = current :: visited in
    let unvisited_neighbors =
      List.filter (fun n -> not (List.mem n visited)) (neighbors graph current)
    in
    let paths = 
      List.filter_map (fun neighbor ->
          match find_shortest_path graph visited neighbor target with
          | Some path -> Some (current :: path)
          | None -> None
        ) unvisited_neighbors
    in
    match paths with
    | [] -> None
    | _ -> Some (List.fold_left (fun acc path -> if List.length path < List.length acc then path else acc) (List.hd paths) paths)

let shortest_path graph start target =
  find_shortest_path graph [] start target

let () =
  match shortest_path graph 1 6 with
  | Some path -> Printf.printf "Shortest path: %s\n" (String.concat " -> " (List.map string_of_int path))
  | None -> Printf.printf "No path found\n"