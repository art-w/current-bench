open Current.Syntax
module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default
module Docker_util = Current_util.Docker_util
module Slack = Current_slack
module Logging = Logging
module Benchmark = Models.Benchmark

let ( >>| ) x f = Current.map f x

let validate_json json_list =
  Current_bench_json.(validate (List.map of_json json_list))

module Source = struct
  type github = {
    token : Fpath.t;
    webhook_secret : string;
    slack_path : Fpath.t option;
    repo : Github.Repo_id.t;
  }

  type t = Github of github | Local of Fpath.t | Github_app of Github.App.t

  let github ~token ~webhook_secret ~slack_path ~repo =
    Github { token; webhook_secret; slack_path; repo }

  let local path = Local path

  let github_app t = Github_app t

  let webhook_secret = function
    | Local _ -> None
    | Github g -> Some g.webhook_secret
    | Github_app g -> Some (Github.App.webhook_secret g)
end

module Docker_config = struct
  type t = {
    cpu : string list;
    numa_node : int option;
    shm_size : int;
    multicore_repositories : string list;
  }

  let v ?(cpu = []) ?numa_node ~shm_size ~multicore_repositories () =
    { cpu; numa_node; shm_size; multicore_repositories }

  let is_multicore ~repository t =
    List.mem (Repository.info repository) t.multicore_repositories

  let cpuset_cpus ~repository t =
    match t.cpu with
    | [] -> []
    | first :: _ ->
        let cpus =
          if is_multicore ~repository t then String.concat "," t.cpu else first
        in
        [ "--cpuset-cpus"; cpus ]

  let cpuset_mems t =
    match t.numa_node with
    | Some i -> [ "--cpuset-mems"; string_of_int i ]
    | None -> []

  let tmpfs t =
    match t.numa_node with
    | Some i ->
        [
          "--tmpfs";
          Fmt.str "/dev/shm:rw,noexec,nosuid,size=%dg,mpol=bind:%d" t.shm_size i;
        ]
    | None ->
        [ "--tmpfs"; Fmt.str "/dev/shm:rw,noexec,nosuid,size=%dg" t.shm_size ]

  let run_args ~repository t =
    [
      "--security-opt";
      "seccomp=./aslr_seccomp.json";
      "--mount";
      "type=volume,src=current-bench-data,dst=/home/opam/bench-dir/current-bench-data";
    ]
    @ tmpfs t
    @ cpuset_cpus ~repository t
    @ cpuset_mems t
end

let pool = Current.Pool.create ~label:"docker" 1

let read_channel_uri p =
  Util.read_fpath p |> String.trim |> Uri.of_string |> Current_slack.channel

let github_status_of_state url = function
  | Ok _ -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m) -> Github.Api.Status.v ~url `Failure ~description:m

let github_set_status ~repository result =
  match Repository.github_head repository with
  | None -> Current.ignore_value result
  | Some head ->
      let status_url = Repository.commit_status_url repository in
      Current.state result
      >>| github_status_of_state status_url
      |> Github.Api.Commit.set_status (Current.return head) "ocaml-benchmarks"
      |> Current.ignore_value

let slack_post ~repository (output : string Current.t) =
  match Repository.slack_path repository with
  | None -> Current.ignore_value output
  | Some path ->
      Current.component "slack post"
      |> let** _ = output in
         let channel = read_channel_uri path in
         Slack.post channel ~key:"output" output

let db_save ~conninfo benchmark output =
  let db = new Postgresql.connection ~conninfo () in
  output
  |> Json_util.parse_many
  |> validate_json
  |> Hashtbl.iter (fun benchmark_name (version, results) ->
         results
         |> List.mapi (fun test_index res ->
                benchmark ~version ~benchmark_name ~test_index res)
         |> List.iter (Models.Benchmark.Db.insert db));
  db#finish

let docker_make_bench ~run_args ~repository image =
  let { Repository.branch; pull_number; _ } = repository
  and repo_info = Repository.info repository
  and commit = Repository.commit_hash repository in
  Docker_util.pread_log ~pool ~run_args image ~repo_info ?pull_number ?branch
    ~commit
    ~args:
      [
        "/usr/bin/setarch";
        "x86_64";
        "--addr-no-randomize";
        "sh";
        "-c";
        "opam exec -- make bench";
      ]

let record_pipeline_stage ~stage ~serial_id ~conninfo image =
  let+ job_id = Current_util.get_job_id image
  and+ state = Current.state image in
  match (job_id, state) with
  | Some job_id, Error (`Active _) ->
      Storage.record_stage_start ~stage ~job_id ~serial_id ~conninfo
  | Some _, Error (`Msg m) ->
      Logs.err (fun log -> log "Error in %s stage: \n%s\n" stage m);
      Storage.record_stage_failure ~stage ~serial_id ~conninfo
  | _ -> ()

let pipeline ~conninfo ~docker_config ~repository =
  let serial_id = Storage.setup_metadata ~repository ~conninfo in
  let src = Repository.src repository in
  let run_args = Docker_config.run_args ~repository docker_config in
  let dockerfile = Custom_dockerfile.dockerfile ~pool ~run_args ~repository in
  let current_image = Docker.build ~pool ~pull:false ~dockerfile (`Git src) in
  let* () =
    record_pipeline_stage ~stage:"build_job_id" ~serial_id ~conninfo
      current_image
  in
  let run_at = Ptime_clock.now () in
  let current_output = docker_make_bench ~run_args ~repository current_image in
  let* () =
    record_pipeline_stage ~stage:"run_job_id" ~serial_id ~conninfo
      current_output
  in
  let+ build_job_id = Current_util.get_job_id current_image
  and+ run_job_id = Current_util.get_job_id current_output
  and+ output = current_output in
  let duration = Ptime.diff (Ptime_clock.now ()) run_at in
  Logs.debug (fun log -> log "Benchmark output:\n%s" output);
  let () =
    db_save ~conninfo
      (Benchmark.make ~duration ~run_at ~repository ?build_job_id ?run_job_id)
      output
  in
  output

let pipeline ~conninfo ~docker_config repository =
  let p = pipeline ~conninfo ~docker_config ~repository in
  let* () = p |> slack_post ~repository |> github_set_status ~repository in
  Current.ignore_value p

let github_repositories ?slack_path repo =
  let* refs =
    Current.component "Get PRs"
    |> let> api, repo = repo in
       Github.Api.refs api repo
  in
  let default_branch = Github.Api.default_ref refs in
  let stale_timestamp = Util.stale_timestamp () in
  let default_branch_name = Util.get_branch_name default_branch in
  let ref_map = Github.Api.all_refs refs in
  let+ _, repo = repo in
  let repository = Repository.v ?slack_path ~name:repo.name ~owner:repo.owner in
  Github.Api.Ref_map.fold
    (fun key head lst ->
      let commit = Github.Api.Commit.id head in
      let repository = repository ~commit ~github_head:head in
      (* If commit is more than two weeks old, then skip it.*)
      if Github.Api.Commit.committed_date head > stale_timestamp then
        match key with
        (* Skip all branches other than the default branch, and check PRs *)
        | `Ref branch when branch = default_branch ->
            repository ~branch:default_branch_name () :: lst
        | `Ref _ -> lst
        | `PR pull_number -> repository ~pull_number () :: lst
      else lst)
    ref_map []

let repositories = function
  | Source.Local path ->
      let local = Git.Local.v path in
      let src = Git.Local.head_commit local in
      let+ head = Git.Local.head local and+ commit = src >>| Git.Commit.id in
      let branch =
        match head with
        | `Commit _ -> None
        | `Ref git_ref -> (
            match String.split_on_char '/' git_ref with
            | [ _; _; branch ] -> Some branch
            | _ ->
                Logs.warn (fun log ->
                    log "Could not extract branch name from: %s" git_ref);
                None)
      in
      [ Repository.v ?branch ~src ~commit ~name:"local" ~owner:"local" () ]
  | Github { repo; slack_path; token; webhook_secret } ->
      let token = token |> Util.read_fpath |> String.trim in
      let api = Current_github.Api.of_oauth ~token ~webhook_secret in
      let repo = Current.return (api, repo) in
      github_repositories ?slack_path repo
  | Github_app app ->
      let+ repos =
        Github.App.installations app
        |> Current.list_map (module Github.Installation) @@ fun installation ->
           let repos = Github.Installation.repositories installation in
           repos
           |> Current.list_map ~collapse_key:"repo"
                (module Github.Api.Repo)
                github_repositories
      in
      List.concat (List.concat repos)

let exists ~conninfo repository =
  let db = new Postgresql.connection ~conninfo () in
  let exists = Benchmark.Db.exists db repository in
  db#finish;
  exists

let process_pipeline ~docker_config ~conninfo ~source () =
  Current.list_iter ~collapse_key:"pipeline"
    (module Repository)
    (fun repo ->
      let* repository = repo in
      if exists ~conninfo repository then Current.ignore_value repo
      else pipeline ~conninfo ~docker_config repository)
    (repositories source)

let v ~current_config ~docker_config ~server:mode ~(source : Source.t) conninfo
    () =
  Db_util.check_connection ~conninfo;
  let pipeline = process_pipeline ~docker_config ~conninfo ~source in
  let engine = Current.Engine.create ~config:current_config pipeline in
  let webhook =
    match Source.webhook_secret source with
    | None -> []
    | Some webhook_secret ->
        let webhook =
          Github.webhook ~engine ~webhook_secret
            ~has_role:Current_web.Site.allow_all
        in
        [ Routes.((s "webhooks" / s "github" /? nil) @--> webhook) ]
  in
  let routes = webhook @ Current_web.routes engine in
  let site =
    Current_web.Site.(v ~has_role:allow_all) ~name:"Benchmarks" routes
  in
  Logging.run
    (Lwt.choose [ Current.Engine.thread engine; Current_web.run ~mode site ])
