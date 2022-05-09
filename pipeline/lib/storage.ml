let setup_metadata ~repository ~worker ~docker_image
    (db : Postgresql.connection) =
  Logs.debug (fun log -> log "Inserting metadata ...");
  let run_at = Db_util.time (Ptime_clock.now ()) in
  let repo_id = Db_util.string (Repository.info repository) in
  let commit = Db_util.string (Repository.commit_hash repository) in
  let branch = Db_util.(option string) (Repository.branch repository) in
  let pull_number = Db_util.(option int) (Repository.pull_number repository) in
  let title = Db_util.(option string) (Repository.title repository) in
  let worker = Db_util.string worker in
  let docker_image = Db_util.string docker_image in
  let query =
    (*
      When setting up metadata, we are only insert the details that we know at th
      beginning. If we see a conflict here, that means we have started running the
      benchmarks again for this repo and commit, so we reset the build_job_id
      and the run_job_id.
    *)
    Fmt.str
      {|INSERT INTO benchmark_metadata
          (run_at, repo_id, commit, branch, pull_number, pr_title, worker, docker_image)
        VALUES (%s, %s, %s, %s, %s, %s, %s, %s)
        ON CONFLICT(repo_id, commit, worker, docker_image)
        DO UPDATE
        SET build_job_id = NULL,
            run_job_id = NULL,
            failed = false,
            cancelled = false,
            success = false,
            reason = ''
        RETURNING id;
      |}
      run_at repo_id commit branch pull_number title worker docker_image
  in
  let result = db#exec query in
  match result#get_all with
  | [| [| id |] |] -> int_of_string id
  | result ->
      Logs.err (fun log ->
          log "Unexpected result while setting up metadata %s:%s\n%a"
            (Repository.info repository)
            (Repository.commit_hash repository)
            (Fmt.array (Fmt.array Fmt.string))
            result);
      -1

let setup_metadata ~repository ~conninfo ~worker ~docker_image =
  Db_util.with_db ~conninfo (setup_metadata ~repository ~worker ~docker_image)

let record_stage_start ~job_id ~serial_id (db : Postgresql.connection) =
  Logs.debug (fun log -> log "Recording build start...");
  let job_id = Db_util.string job_id in
  let serial_id = Db_util.int serial_id in
  let query =
    Fmt.str
      {|UPDATE benchmark_metadata
        SET build_job_id = %s, run_job_id = %s
        WHERE id = %s
      |}
      job_id job_id serial_id
  in
  ignore (db#exec ~expect:[ Postgresql.Command_ok ] query)

let record_stage_start ~job_id ~serial_id ~conninfo =
  Db_util.with_db ~conninfo (record_stage_start ~job_id ~serial_id)

let record_stage_failure ~serial_id ~reason (db : Postgresql.connection) =
  Logs.debug (fun log -> log "Recording stage failure...");
  let serial_id = Db_util.int serial_id in
  let query =
    Fmt.str
      {|UPDATE benchmark_metadata
        SET failed = true,
            reason = %s
        WHERE id = %s
      |}
      (Db_util.string reason) serial_id
  in
  ignore (db#exec ~expect:[ Postgresql.Command_ok ] query)

let record_stage_failure ~serial_id ~reason ~conninfo =
  Db_util.with_db ~conninfo (record_stage_failure ~serial_id ~reason)

let record_cancel ~serial_id ~reason (db : Postgresql.connection) =
  Logs.debug (fun log -> log "Recording stage cancel...");
  let serial_id = Db_util.int serial_id in
  let query =
    Fmt.str
      {|UPDATE benchmark_metadata
        SET cancelled = true,
            reason = %s
        WHERE id = %s
      |}
      (Db_util.string reason) serial_id
  in
  ignore (db#exec ~expect:[ Postgresql.Command_ok ] query)

let record_cancel ~serial_id ~reason ~conninfo =
  Db_util.with_db ~conninfo (record_cancel ~serial_id ~reason)

let record_success ~serial_id (db : Postgresql.connection) =
  Logs.debug (fun log -> log "Recording stage success...");
  let serial_id = Db_util.int serial_id in
  let query =
    Fmt.str
      {|UPDATE benchmark_metadata
        SET success = true
        WHERE id = %s
      |}
      serial_id
  in
  ignore (db#exec ~expect:[ Postgresql.Command_ok ] query)

let record_success ~serial_id ~conninfo =
  Db_util.with_db ~conninfo (record_success ~serial_id)

let mark_closed_pull_requests ~open_pulls (db : Postgresql.connection) =
  Logs.debug (fun log -> log "Updating open and closed pulls...");
  let open_pr_query =
    String.concat " OR "
    @@ List.map
         (fun (repo_id, pull_number) ->
           Fmt.str {|(repo_id = '%s' AND pull_number = %d)|} repo_id pull_number)
         open_pulls
  in
  let open_pr_query =
    if open_pr_query <> "" then open_pr_query else "FALSE"
  in
  let query =
    Fmt.str
      {|UPDATE benchmark_metadata
        SET is_open_pr = pull_number is NULL OR %s;
      |}
      open_pr_query
  in
  ignore (db#exec ~expect:[ Postgresql.Command_ok ] query)

let mark_closed_pull_requests ~open_pulls ~conninfo =
  Db_util.with_db ~conninfo (mark_closed_pull_requests ~open_pulls)


let get_projects (db : Postgresql.connection) =
  let query = {|SELECT DISTINCT repo_id FROM benchmark_metadata|} in
  let results = db#exec query in
  let results = results#get_all in
  Array.to_list results
  |> List.map (function [| repo_id |] -> repo_id | _ -> failwith "?")

let get_projects ~db = Db_util.with_db ~conninfo:db get_projects

let get_repos ~owner (db : Postgresql.connection) =
  let owner = Db_util.string (owner ^ "/%") in
  let query =
    Fmt.str
      {|SELECT DISTINCT repo_id
        FROM benchmark_metadata
        WHERE repo_id LIKE %s
      |}
      owner
  in
  let results = db#exec query in
  let results = results#get_all in
  Array.to_list results
  |> List.map (function [| repo_id |] -> repo_id | _ -> failwith "?")

let get_repos ~db ~owner = Db_util.with_db ~conninfo:db (get_repos ~owner)

let get_prs ~repo_id (db : Postgresql.connection) =
  let repo_id = Db_util.string repo_id in
  let query =
    Fmt.str
      {|SELECT b.pull_number, b.worker, b.docker_image, b.pr_title, b.run_at, b.success, b.cancelled, b.reason
        FROM (SELECT pull_number, worker, docker_image, MAX(run_at) AS run_at
              FROM benchmark_metadata
              WHERE pull_number IS NOT NULL
                AND repo_id=%s
                AND is_open_pr
              GROUP BY pull_number, worker, docker_image) AS p
        JOIN benchmark_metadata AS b
        ON ((p.pull_number, p.worker, p.docker_image, p.run_at)
            = (b.pull_number, b.worker, b.docker_image, b.run_at))
      |}
      repo_id
  in
  let results = db#exec query in
  let results = results#get_all in
  Printf.printf "got %i\n%!" (Array.length results);
  Array.to_list results
  |> List.map (function [| pr ; worker ; docker_image ; title ; run_at ; success ; cancelled ; reason |] ->
                 let status = match success, cancelled, reason with
                 | "t", "f", "" -> "OK"
                 | "f", "t", _ -> "CANCELLED: " ^ reason
                 | "f", "f", _ -> "ERROR: " ^ reason
                 | _ -> Printf.sprintf "%S %S %S\n%!" success cancelled reason
                 in
                 ((pr, worker, docker_image), title, run_at, status)
               | _ -> failwith "?")

let get_prs ~db repo_id = Db_util.with_db ~conninfo:db (get_prs ~repo_id)

let get_workers ~repo_id ~pr (db : Postgresql.connection) =
  let repo_id = Db_util.string repo_id in
  let pr = match pr with
    | `PR pr -> "pull_number = " ^ Db_util.int (int_of_string pr)
    | `Branch -> "pull_number IS NULL"
  in
  let query =
    Fmt.str
      {|SELECT DISTINCT worker, docker_image
        FROM benchmark_metadata
        WHERE repo_id = %s
          AND (%s)
      |}
      repo_id
      pr
  in
  let results = db#exec query in
  let results = results#get_all in
  Printf.printf "got %i\n%!" (Array.length results);
  Array.to_list results
  |> List.map (function [| worker ; docker_image |] -> (worker, docker_image)
               | _ -> failwith "?")

let get_workers ~db ~repo_id ~pr =
  Db_util.with_db ~conninfo:db (get_workers ~repo_id ~pr)



let get_benchmarks ~repo_id ~worker ~docker_image ~pr (db : Postgresql.connection) =
  let repo_id = Db_util.string repo_id in
  let docker_image = Db_util.string docker_image in
  let worker = Db_util.string worker in
  let pr = match pr with
    | `PR pr -> "pull_number IS NULL OR pull_number = " ^ Db_util.int (int_of_string pr)
    | `Branch -> "pull_number IS NULL"
  in
  let query =
    Fmt.str
      {|SELECT benchmark_name, test_name, commit, metrics
        FROM benchmarks
        WHERE repo_id = %s
          AND (%s)
          AND worker = %s
          AND docker_image = %s
        ORDER BY run_at ASC
      |}
      repo_id
      pr
      worker
      docker_image
  in
  Printf.printf "query = %s\n%!" query ;
  let results = db#exec query in
  let results = results#get_all in
  Printf.printf "got %i\n%!" (Array.length results);
  Array.to_list results
  |> List.map (function [| benchmark_name ; test_name ; commit ; json |] ->
      (*
      let commit = match String.split_on_char '.' commit with
        | hd :: _ -> hd
        | _ -> commit
      in
      *)
let commit = String.sub commit 0 6 in
                  Printf.printf "commit = %S\n%!" commit ;
                  (benchmark_name, test_name, commit, json)
               | _ -> failwith "?")

let get_benchmarks ~db ~repo_id ~pr ~worker ~docker_image =
  Db_util.with_db ~conninfo:db (get_benchmarks ~repo_id ~pr ~worker ~docker_image)
