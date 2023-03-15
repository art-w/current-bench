let factors = [ ("ns", 1.0); ("ms", 1000.0) ]

let convert ~from ~target value =
  if from = target
  then value
  else
    try
      let from_factor = List.assoc from factors in
      let target_factor = List.assoc target factors in
      value *. target_factor /. from_factor
    with Not_found -> value

let convert_value ~from ~target = function
  | Current_bench_json.V2.Float f ->
      Current_bench_json.V2.Float (convert ~from ~target f)
  | Floats f -> Floats (List.map (convert ~from ~target) f)
  | Assoc f ->
      Assoc (List.map (fun (lbl, f) -> (lbl, convert ~from ~target f)) f)

let convert_metric ~target metric =
  let open Current_bench_json.V2 in
  {
    metric with
    units = target.units;
    value = convert_value ~from:metric.units ~target:target.units metric.value;
  }
