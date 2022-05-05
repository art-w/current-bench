open Lwt.Syntax

type config = {
  url : string;
  port : int;
}

module H = Tyxml.Html

let list_projects ~conninfo () =
  let ps = Storage.get_projects ~conninfo in
  H.ul @@ List.map (fun p -> H.li [H.a ~a:[H.a_href ("/" ^ p)] [H.txt p]]) ps

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
      let subvalues = try String_map.find subkey values with Not_found -> [] in
      let values = String_map.add subkey ((metric, commit) :: subvalues) values in
      String_map.add key values acc)
    String_map.empty
    lst

let metric_value m =
  let open Current_bench_json.V2 in
  match m.value with
  | Float x -> x
  | Floats xs -> List.fold_left ( +. ) 0.0 xs /. float (List.length xs)
  | Assoc _ -> failwith "todo assoc"

let list_benchmarks ~conninfo ~repo_id ~pr ~worker ~docker_image () =
  let ps = Storage.get_benchmarks ~conninfo ~repo_id ~pr ~worker ~docker_image in
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

                let short str = String.sub str 0 6 in

                let plotly =
                  metrics
                  |> String_map.bindings
                  |> List.map (fun (subkey, metrics) ->
                        let xs =
                          (*
                          `List (List.mapi (fun i _ -> `Int (i+1)) metrics)
                          *)

                          `List (List.mapi (fun _ (_, commit) -> `String (short commit)) metrics)
                        in
                        let ys =
                          `List (List.map (fun (metric, _) -> `Float (metric_value metric)) metrics)
                        in

                        let plotly =
                            `Assoc [
                              "x", xs;
                              "y", ys;
                              "name", `String subkey;
                              "type", `String "scatter";
                              (*
                              "line", `Assoc [ "shape", `String "hv" ];
                              *)
                              (* "mode", `String "lines+marker"; *)
                            ]
                        in
                      plotly)
                  |> (fun xs -> `List xs)
                in


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

let list_pr ~conninfo ~repo_id ~worker ~docker_image () =
  let ps = Storage.get_prs ~conninfo repo_id in
  H.div [

    list_benchmarks ~conninfo ~repo_id ~pr:`Branch ~worker ~docker_image () ;
    H.hr () ;

    H.ul
    @@ List.map (fun ((pr, worker, docker_image), title, run_at, status) ->
         H.li [H.a ~a:[H.a_href ("/" ^ repo_id ^ "/pull/" ^ pr ^ "?worker=" ^ worker ^ "&docker_image=" ^ docker_image)]
                   [H.txt (status ^ " PR#" ^ pr ^ ": " ^ title ^ " (" ^ run_at ^ ") " ^ " on " ^ worker ^ "/" ^ docker_image)]])
    @@ ps ;
  ]




let template body =
  H.html
    ~a:[ H.a_lang "en" ]
    (H.head
       (H.title (H.txt "current-bench"))
       [ H.meta ~a:[ H.a_charset "UTF-8" ] ()
       ; H.meta ~a:[ H.a_name "viewport"; H.a_content "width=device-width, initial-scale=1" ] ()
       ; H.script ~a:[ H.a_src "https://cdn.plot.ly/plotly-2.11.1.min.js"] (H.txt "")
       ; H.script ~a:[ H.a_src "/_static/plot.js" ] (H.txt "")
       ; H.link ~rel:[ `Stylesheet ] ~href:"/_static/style.css" ()
       (*
       ; link ~rel:[ `Icon ] ~href:"favicon.ico" ()
    *)
       ])
  @@ H.body
       [
         H.div ~a:[] [ body ]
       ]


let string_of_tyxml html = Format.asprintf "%a" (Tyxml.Html.pp ()) html


let get_worker ~conninfo ~repo_id ~pr request =
  let worker = Dream.query request "worker" in
  let docker_image = Dream.query request "docker_image" in
  let candidates = Storage.get_workers ~conninfo ~repo_id ~pr in
  match worker, docker_image, candidates with
  | Some w, Some i, _ when List.mem (w, i) candidates -> (w, i)
  | _, _, [] -> Config.default_worker, Config.default_docker
  | _, _, wi::_ -> wi

let html body =
  Dream.html (string_of_tyxml (template (body ())))

let main ~front ~conninfo =
  Dream.serve ~port:front.port ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.router [

      Dream.get "/_static/**" (Dream.static "/mnt/project/static");

      Dream.get "/" (fun _ -> html (list_projects ~conninfo));

      Dream.get "/:owner/:repo" (fun request ->
        Printf.printf "cwd = %S\n%!" (Sys.getcwd ());
        let owner = Dream.param request "owner" in
        let repo = Dream.param request "repo" in
        let repo_id = owner ^ "/" ^ repo in
        let worker, docker_image = get_worker ~conninfo ~repo_id ~pr:`Branch request in
        html (list_pr ~conninfo ~repo_id ~worker ~docker_image));

      Dream.get "/:owner/:repo/pull/:pr" (fun request ->
        let owner = Dream.param request "owner" in
        let repo = Dream.param request "repo" in
        let repo_id = owner ^ "/" ^ repo in
        let pr = `PR (Dream.param request "pr") in
        let worker, docker_image = get_worker ~conninfo ~repo_id ~pr request in
        html (list_benchmarks ~conninfo ~repo_id ~pr ~worker ~docker_image))
  ]

let main ~front ~conninfo =
  let+ () = main ~front ~conninfo in
  Error (`Msg "Frontend was terminated?")
