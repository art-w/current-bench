module H = Tyxml.Html

module String_map = Map.Make (struct
  type t = string
  let compare = String.compare
end)

module M = Map.Make (struct
  type t = string * string
  let compare = Stdlib.compare
end)

let group_by_test lst =
  M.bindings @@
  List.fold_left
    (fun acc (benchmark_name, test_name, commit, json) ->
      let key = (benchmark_name, test_name) in
      let values = try M.find key acc with Not_found -> [] in
      let values = (commit, json) :: values in
      M.add key values acc)
    M.empty
    lst

let group_by_metric lst =
  String_map.bindings @@
  List.fold_left
    (fun acc (metric, commit) ->
      let key = metric.Current_bench_json.V2.name in
      let key, subkey = match String.split_on_char '/' key with
        | [key; subkey] -> key, subkey
        | [key] -> key, ""
        | _ -> failwith "too many /"
      in
      let values = try String_map.find key acc with Not_found -> String_map.empty in
      let subcolor, subvalues =
        try String_map.find subkey values
        with Not_found ->
          let color = { Color.h = Random.float 1.0 ; s = 1.0 ; l = 0.4 ; a = 1.0 } in
          color, [] in
      let values = String_map.add subkey (subcolor, (metric, commit) :: subvalues) values in
      String_map.add key values acc)
    String_map.empty
    lst

let minimum = function
  | [] -> 0.0
  | x :: xs -> List.fold_left min x xs
let maximum = function
  | [] -> 0.0
  | x :: xs -> List.fold_left max x xs
let mean = function
  | [] -> 0.0
  | xs -> List.fold_left ( +. ) 0.0 xs /. float (List.length xs)

let metric_value f m =
  let open Current_bench_json.V2 in
  match m.value with
  | Float x -> x
  | Floats xs -> f xs (* List.fold_left ( +. ) 0.0 xs /. float (List.length xs) *)
  | Assoc _ -> failwith "todo assoc"

let plot ~db ~repo_id ~pr ~worker ~docker_image =
  let ps = Storage.get_benchmarks ~db ~repo_id ~pr ~worker ~docker_image in
  let timeline =
    List.fold_left
      (fun (acc, keep) (_, _, commit, _) ->
        if String_map.mem commit acc
        then (acc, keep)
        else (String_map.add commit () acc, commit :: keep)
      )
      (String_map.empty, [])
      ps
    |> snd
    (* TODO: must resort somehow?! *)
  in
  let ps = group_by_test ps in
  H.div ~a:[H.a_class ["benchmarks"]]
  @@ List.map (fun ((benchmark_name, test_name), values) ->

        let metrics =
          List.concat @@
          List.map
            (fun (commit, json_str) ->
                let jsons = Yojson.Safe.from_string json_str in
                let metrics = Current_bench_json.V2.metrics_of_json [] jsons in
                List.map (fun m -> m, commit) metrics
            )
            values
        in
        let metrics = group_by_metric metrics in

        H.div ~a:[H.a_class ["benchmark"]] [
          H.h1 ~a:[] [H.txt (benchmark_name ^ " -- " ^ test_name)] ;
          H.div
          @@ List.map (fun (metric_name, metrics) ->

            (*
                let short str = String.sub str 0 6 in
*)

                let plotly_err =
                  metrics
                  |> String_map.bindings
                  |> List.map (fun (subkey, (color, metrics)) ->

                      (*
                        let xs =
                          (*
                          `List (List.mapi (fun i _ -> `Int (i+1)) metrics)
                          *)

                          List.mapi (fun _ (_, commit) -> `String (commit)) metrics
                        in
                        let xs = xs @ List.rev xs in
                        let ys =
                          List.map (fun (metric, _) -> `Float (metric_value maximum metric)) metrics
                        @
                          List.rev (
                            List.map (fun (metric, _) -> `Float (metric_value minimum metric)) metrics
                          )
                        in
                    *)

                        let values =
                          List.fold_left
                            (fun acc (metric, commit) ->
                              String_map.add commit (metric_value minimum metric, metric_value maximum metric) acc
                            )
                            String_map.empty
                            metrics
                        in
                        let xs =
                          (*
                          `List (List.mapi (fun i _ -> `Int (i+1)) metrics)
                          *)
                          List.map (fun commit -> `String commit) (timeline @ List.rev timeline)

                          (*
                          `List (List.mapi (fun _ (_, commit) -> `String (commit)) metrics)
                  *)
                        in
                        let ys_min =
                          timeline
                          |> List.map (fun commit ->
                               try `Float (fst (String_map.find commit values))
                               with Not_found -> `Null)
                          (*
                          List.map (fun (metric, _) ->
                            `Float (metric_value mean metric)) metrics
                            *)
                        in
                        let ys_max =
                          timeline
                          |> List.rev
                          |> (function (_::xs) -> xs | [] -> [])
                          |> List.map (fun commit ->
                               try `Float (snd (String_map.find commit values))
                               with Not_found -> `Null)
                          (*
                          List.map (fun (metric, _) ->
                            `Float (metric_value mean metric)) metrics
                            *)
                        in

                        let ys = ys_min @ ys_max in

                        `Assoc [
                          "x", `List xs ;
                          "y", `List ys ;
                          "name", `String subkey;
                          "type", `String "scatter";
                          "line", `Assoc [ "color", `String "transparent";
                                           "shape", `String "hv";
                                         ];
                          "showlegend", `Bool false;
                          "fill", `String "tozerox";
                          "fillcolor", `String (Color.to_css { color with Color.a = 0.2 }) ;
                          "hoverinfo", `String "none";
                          (* "mode", `String "lines+marker"; *)
                        ]
                  )
                in
                let plotly_mean =
                  metrics
                  |> String_map.bindings
                  |> List.map (fun (subkey, (color, metrics)) ->

                        let values =
                          List.fold_left
                            (fun acc (metric, commit) ->
                              String_map.add commit (metric_value mean metric) acc
                            )
                            String_map.empty
                            metrics
                        in
                        let xs =
                          (*
                          `List (List.mapi (fun i _ -> `Int (i+1)) metrics)
                          *)
                          `List (List.map (fun commit -> `String commit) timeline)

                          (*
                          `List (List.mapi (fun _ (_, commit) -> `String (commit)) metrics)
                  *)
                        in
                        let ys_mean =
                          timeline
                          |> List.map (fun commit ->
                               try `Float (String_map.find commit values)
                               with Not_found -> `Null)
                          (*
                          List.map (fun (metric, _) ->
                            `Float (metric_value mean metric)) metrics
                            *)
                        in
                        let plotly : Yojson.Safe.t =
                            `Assoc [
                              "x", xs ;
                              "y", `List ys_mean;
                              "name", `String subkey;
                              "type", `String "scatter";
                              "line", `Assoc [ "color", `String (Color.to_css color);
                                               "shape", `String "hv";
                                             ];
                              "showlegend", `Bool (subkey <> "");
                              (* "mode", `String "lines+marker"; *)
                            ];
                        in
                      plotly)
                in
                let plotly = `List (plotly_err @ plotly_mean) in


                H.div ~a:[H.a_class ["metrics"]] [
                  H.h3 [H.txt metric_name];
                  H.div [];
                  H.div ~a:[H.a_class ["current-bench_plot"]] [
                    H.pre [H.txt (Yojson.Safe.pretty_to_string plotly)];
                  ]
                ]

                (*
                H.li [
                  H.h3 [H.txt metric_name] ;
                  H.ul @@
                  List.map (fun (metric, commit) ->
                      H.li [H.txt (commit ^ " => " ^ string_of_float (metric_value metric))]
                  )
                  @@ metrics
                ]
                *)
             )
          @@ metrics
        ])
  @@ ps
