open Lwt.Syntax

type config = {
  url : string;
  port : int;
}

module H = Tyxml.Html

let list_projects ~db () =
  let ps = Storage.get_projects ~db in
  H.ul @@ List.map (fun p -> H.li [H.a ~a:[H.a_href ("/" ^ p)] [H.txt p]]) ps


let list_benchmarks ~db ~repo_id ~pr ~worker ~docker_image () =
  Plot.plot ~db ~repo_id ~pr ~worker ~docker_image

let list_repos ~db ~owner () =
  let ps = Storage.get_repos ~db ~owner in
  H.div [
    H.ul
    @@ List.map (fun repo_id ->
         H.li [H.a ~a:[H.a_href ("/" ^ repo_id)] [H.txt repo_id]])
    @@ ps ;
  ]


let list_pr ~db ~repo_id ~worker ~docker_image () =
  let ps = Storage.get_prs ~db repo_id in
  H.div [

    H.ul
    @@ List.map (fun ((pr, worker, docker_image), title, run_at, status) ->
         H.li [H.a ~a:[H.a_href ("/" ^ repo_id ^ "/pull/" ^ pr ^ "?worker=" ^ worker ^ "&docker_image=" ^ docker_image)]
                   [H.txt (status ^ " PR#" ^ pr ^ ": " ^ title ^ " (" ^ run_at ^ ") " ^ " on " ^ worker ^ "/" ^ docker_image)]])
    @@ ps ;

    H.hr () ;

    list_benchmarks ~db ~repo_id ~pr:`Branch ~worker ~docker_image () ;
  ]



let header ~request =
  H.div ~a:[H.a_id "header"] [
    H.a ~a:[H.a_href "/"] [H.txt "current-bench"];
    (match Dream.param request "owner" with
    | owner -> H.a ~a:[H.a_href ("/" ^ owner)] [H.txt owner];
    | exception _ -> H.txt ""
    );
    (match Dream.param request "owner", Dream.param request "repo" with
    | owner, repo -> H.a ~a:[H.a_href ("/" ^ owner ^ "/" ^ repo)] [H.txt repo];
    | exception _ -> H.txt ""
    );
  ]

let template ~request body =
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
         header ~request;
         H.div ~a:[] [ body ]
       ]


let string_of_tyxml html = Format.asprintf "%a" (Tyxml.Html.pp ()) html


let get_worker ~db ~repo_id ~pr request =
  let worker = Dream.query request "worker" in
  let docker_image = Dream.query request "docker_image" in
  let candidates = Storage.get_workers ~db ~repo_id ~pr in
  match worker, docker_image, candidates with
  | Some w, Some i, _ when List.mem (w, i) candidates -> (w, i)
  | _, _, [] -> Config.default_worker, Config.default_docker
  | _, _, wi::_ -> wi

let html ~request body =
  Dream.html (string_of_tyxml (template ~request (body ())))

let main ~front ~db =
  Dream.serve ~port:front.port ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.router [

      Dream.get "/_static/**" (Dream.static "/mnt/project/static");

      Dream.get "/" (fun request -> html ~request (list_projects ~db));

      Dream.get "/:owner" (fun request ->
        let owner = Dream.param request "owner" in
        (* let worker, docker_image = get_worker ~db ~repo_id ~pr:`Branch request in *)
        html ~request (list_repos ~db ~owner));

      Dream.get "/:owner/:repo" (fun request ->
        let owner = Dream.param request "owner" in
        let repo = Dream.param request "repo" in
        let repo_id = owner ^ "/" ^ repo in
        let worker, docker_image = get_worker ~db ~repo_id ~pr:`Branch request in
        html ~request (list_pr ~db ~repo_id ~worker ~docker_image));

      Dream.get "/:owner/:repo/pull/:pr" (fun request ->
        let owner = Dream.param request "owner" in
        let repo = Dream.param request "repo" in
        let repo_id = owner ^ "/" ^ repo in
        let pr = `PR (Dream.param request "pr") in
        let worker, docker_image = get_worker ~db ~repo_id ~pr request in
        html ~request (list_benchmarks ~db ~repo_id ~pr ~worker ~docker_image))
  ]

let main ~front ~db =
  let+ () = main ~front ~db in
  Error (`Msg "Frontend was terminated?")
