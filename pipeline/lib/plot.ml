module H = Tyxml.Html

module Commits = Map.Make (String)
module Submetrics = Map.Make (String)
module Metrics = Map.Make (String)
module Tests = Map.Make (String)
module Benchmarks = Map.Make (String)

module String_map = Map.Make (struct
  type t = string
  let compare = String.compare
end)

module M = Map.Make (struct
  type t = string * string
  let compare = Stdlib.compare
end)

type commit = string

type value = Current_bench_json.V2.value
type values_by_commit = value Commits.t (* indexed by commits!! *)

type values = values_by_commit Submetrics.t

type m = {
  units : string ;
  values : values ;
  (* etc *)
}

type t = {
  timeline : commit list ;
  benchmarks : m Metrics.t Tests.t Benchmarks.t ; (* benchmark_name -> test_name -> metric_name -> ? *)
}

let empty = { timeline = [] ; benchmarks = Benchmarks.empty }

let add commit subkey value = function
  (*
  | Single vs ->
      assert (subkey = "") ;
      Single (String_map.add commit value vs)
  | Multiple ms ->
      assert (subkey <> "") ;
  *)
  | ms ->
      Printf.printf "- ADD subkey = %S\n%!" subkey ;
      let vs = try Submetrics.find subkey ms with Not_found -> Commits.empty in
      let vs = Commits.add commit value vs in
      let ms = Submetrics.add subkey vs ms in
      (* Multiple *) ms 

let add (benchmark_name, test_name, commit, json) t =
  let tests = try Benchmarks.find benchmark_name t with Not_found -> Tests.empty in
  let metrics = try Tests.find test_name tests with Not_found -> Metrics.empty in
  let metrics =
    List.fold_left
      (fun acc metric ->
        let name = metric.Current_bench_json.V2.name in
        let name, subkey = match String.split_on_char '/' name with
          | [name; subkey] -> name, subkey
          | _ -> name, ""
        in
        let value = metric.Current_bench_json.V2.value in
        let m =
          try let m = Metrics.find name acc in
              Printf.printf "ok found %S\n%!" name ;
              assert (metric.Current_bench_json.V2.units = m.units) ; (* TODO *)
              { m with values = add commit subkey value m.values }
          with Not_found ->
            Printf.printf "- NOTFOUND %S subkey = %S\n%!" name subkey ;
            let value = Commits.singleton commit value in
            { units = metric.Current_bench_json.V2.units
            ; values = (* match subkey with
                | "" -> Single value
                | _  -> Multiple *) (Submetrics.singleton subkey value)
            }
        in
        Metrics.add name m acc)
      metrics
      json
  in
  let tests = Tests.add test_name metrics tests in
  Benchmarks.add benchmark_name tests t


(*
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
*)

(*
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
      let values = try String_map.find key acc with Not_found -> Submetrics.empty in
      let subcolor, subvalues =
        try Submetrics.find subkey values
        with Not_found ->
          let color = { Color.h = Random.float 1.0 ; s = 1.0 ; l = 0.4 ; a = 1.0 } in
          color, [] in
      let values = Submetrics.add subkey (subcolor, (metric, commit) :: subvalues) values in
      String_map.add key values acc)
    String_map.empty
    lst
*)

(*
let minimum = function
  | [] -> 0.0
  | x :: xs -> List.fold_left min x xs
let maximum = function
  | [] -> 0.0
  | x :: xs -> List.fold_left max x xs
let mean = function
  | [] -> 0.0
  | xs -> List.fold_left ( +. ) 0.0 xs /. float (List.length xs)
*)

let metric_value f m =
  let open Current_bench_json.V2 in
  match m.value with
  | Float x -> x
  | Floats xs -> f xs (* List.fold_left ( +. ) 0.0 xs /. float (List.length xs) *)
  | Assoc _ -> failwith "todo assoc"

let minimum = function
  | Current_bench_json.V2.Float v -> v
  | Floats vs -> List.fold_left min 0.0 vs
  | _ -> failwith "todo assoc"

let maximum = function
  | Current_bench_json.V2.Float v -> v
  | Floats vs -> List.fold_left max 0.0 vs
  | _ -> failwith "todo assoc"

let average = function
  | Current_bench_json.V2.Float v -> v
  | Floats vs -> List.fold_left ( +. ) 0.0 vs /. float (List.length vs)
  | _ -> failwith "todo assoc"


let plot1 ~xs ~timeline ~subkey vs =
  Printf.printf "- subkey = %S\n%!" subkey ;
      let ys =
        List.map
          (fun commit ->
            match Commits.find commit vs with
            | values ->
                `Float (minimum values), `Float (average values), `Float (maximum values)
            | exception Not_found -> `Null, `Null, `Null
          )
          timeline
      in

      let ys_min = List.map (fun (y, _, _) -> y) ys in
      let ys_avg = List.map (fun (_, y, _) -> y) ys in
      let ys_max = List.map (fun (_, _, y) -> y) ys in

          let color = { Color.h = Random.float 1.0 ; s = 1.0 ; l = 0.4 ; a = 1.0 } in

      let errors =
        let ys = ys_min @ List.rev ys_max in

        `Assoc [
          "x", `List (xs @ List.rev xs) ;
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
      in

      errors,
      `Assoc [
        "x", `List xs ;
        "y", `List ys_avg ;
        "name", `String subkey;
        "type", `String "scatter";
        "line", `Assoc [ "color", `String (Color.to_css color);
                         "shape", `String "hv";
                       ];
                       (*
        "showlegend", `Bool false;
        "fill", `String "tozerox";
        "fillcolor", `String (Color.to_css { color with Color.a = 0.2 }) ;
        "hoverinfo", `String "none";
        *)
        (* "mode", `String "lines+marker"; *)
      ]

let plot ~xs ~timeline metrics =
  let _units = metrics.units in
  let values = metrics.values in
  let plots =
    Submetrics.bindings values
    |> List.map (fun (subkey, vs) -> plot1 ~xs ~timeline ~subkey vs)
  in
  let bg, lines = List.split plots in
  let plotly = `List (List.rev_append bg lines) in
  H.pre [H.txt (Yojson.Safe.pretty_to_string plotly)]

let plot_metrics ~xs ~timeline metrics =
   Metrics.bindings metrics
   |> List.map (fun (plot_name, metrics) ->
  H.div [
    H.h4 [H.txt plot_name] ;
    H.div [];
    H.div ~a:[H.a_class ["current-bench_plot"]] [ plot ~xs ~timeline metrics ]
  ])

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
  in

  let benchmarks =
    List.fold_left
      (fun acc (name, test, commit, json_str) ->

        let jsons = Yojson.Safe.from_string json_str in
        let metrics = Current_bench_json.V2.metrics_of_json [] jsons in
        let entry = (name, test, commit, metrics) in

        add entry acc)
      Benchmarks.empty
      ps
  in

  let xs = List.map (fun commit -> `String (String.sub commit 0 6)) timeline in

  (* let ps = group_by_test ps in *)
  H.div ~a:[H.a_class ["benchmarks"]]
  @@ List.map (fun (benchmark_name, tests) ->
      H.div ~a:[H.a_class ["benchmark"]] [
        H.h2 [H.txt benchmark_name] ;
        H.div (
      Tests.bindings tests
      |> List.map (fun (test_name, metrics) ->

          H.div [
            H.h3 [H.txt test_name] ;
            H.div (plot_metrics ~xs ~timeline metrics)
          ]
      ))])

(*


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
  *)
  @@ (Benchmarks.bindings benchmarks)
