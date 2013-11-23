open Core.Std

let (/) = Filename.concat

let getenv var =
  try Sys.getenv_exn var
  with Not_found -> ""

module Engine = struct
  let initial_candidates = 
    let aux s =
      try List.map (Array.to_list (Sys.readdir s)) ~f:(fun s -> s, 0)
      with _ -> []
    in
    List.bind (String.split (getenv "PATH") ~on:':') aux
    |> List.sort ~cmp:(fun (s1, _) (s2, _) -> String.compare s1 s2)

  (* let current_source = ref "" *)

  let current_candidates = ref initial_candidates

  let match_strict = function
    | "" -> !current_candidates
    | str ->
      let pattern = Str.regexp str in
      let result =
        List.filter_map !current_candidates ~f:(fun (candidate, start) ->
          if not (Str.string_match pattern candidate start) then None else
          Some (candidate, Str.match_end ())
        )
      in
      current_candidates := result ;
      result

  let get_candidates new_input =
    current_candidates := initial_candidates ;
    List.map (match_strict new_input) ~f:fst

  let filter_candidates_strict c =
    let result =
      List.filter_map !current_candidates ~f:(fun (candidate, start) ->
        if String.length candidate <= start || candidate.[start] <> c then None else
        Some (candidate, start + 1)
      )
    in
    current_candidates := result ;
    result

  let filter_candidates c = List.map (filter_candidates_strict c) ~f:fst

  let complete str =
    match !current_candidates with
    | [] -> str
    | (x, _) :: _ -> x
end

include Instance.Run (struct
  let initial_prompt = ""

  include Engine
end)
