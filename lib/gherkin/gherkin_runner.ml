(** Gherkin runner — executes .feature scenarios against step definitions.

    Step definitions are registered with regex patterns. When a step
    matches, the captured groups are passed as arguments. *)

module G = Gherkin_ast

type 'ctx step_def = {
  pattern : Re.re;
  pattern_src : string;
  handler : 'ctx -> string list -> 'ctx;
}

type 'ctx t = {
  given_steps : 'ctx step_def list ref;
  when_steps : 'ctx step_def list ref;
  then_steps : 'ctx step_def list ref;
}

let create () =
  {
    given_steps = ref [];
    when_steps = ref [];
    then_steps = ref [];
  }

let compile_pattern pattern_src = Re.Perl.compile_pat pattern_src

let given t pattern_src handler =
  t.given_steps := !(t.given_steps) @ [{ pattern = compile_pattern pattern_src; pattern_src; handler }]

let when_ t pattern_src handler =
  t.when_steps := !(t.when_steps) @ [{ pattern = compile_pattern pattern_src; pattern_src; handler }]

let then_ t pattern_src handler =
  t.then_steps := !(t.then_steps) @ [{ pattern = compile_pattern pattern_src; pattern_src; handler }]

let step_defs_for t (kw : G.step_keyword) =
  match kw with
  | Given | And | But -> !(t.given_steps) @ !(t.when_steps) @ !(t.then_steps)
  | When -> !(t.when_steps) @ !(t.given_steps) @ !(t.then_steps)
  | Then -> !(t.then_steps) @ !(t.given_steps) @ !(t.when_steps)

let extract_groups re text =
  match Re.exec_opt re text with
  | None -> None
  | Some group ->
      let n = Re.Group.nb_groups group in
      Some (List.init (n - 1) (fun i ->
        try Re.Group.get group (i + 1) with Not_found -> ""))

let find_step_def t keyword text =
  let rec try_defs = function
    | [] -> None
    | def :: rest ->
        match extract_groups def.pattern text with
        | Some groups -> Some (def, groups)
        | None -> try_defs rest
  in
  try_defs (step_defs_for t keyword)

type run_result = {
  feature_name : string;
  total_scenarios : int;
  passed : int;
  failed : int;
  errors : (string * string) list;
}

let run_scenario t ~make_ctx (sc : G.scenario) =
  let ctx = ref (make_ctx ()) in
  try
    List.iter
      (fun (step : G.step) ->
        match find_step_def t step.keyword step.text with
        | Some (def, groups) -> ctx := def.handler !ctx groups
        | None ->
            failwith
              (Printf.sprintf "No step definition for: %s %s"
                 (G.show_step_keyword step.keyword) step.text))
      sc.steps;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

let run_feature t ~make_ctx (feat : G.feature) : run_result =
  let errors = ref [] in
  let passed = ref 0 in
  List.iter
    (fun (sc : G.scenario) ->
      match run_scenario t ~make_ctx sc with
      | Ok () -> incr passed
      | Error msg -> errors := (sc.name, msg) :: !errors)
    feat.scenarios;
  {
    feature_name = feat.name;
    total_scenarios = List.length feat.scenarios;
    passed = !passed;
    failed = List.length !errors;
    errors = List.rev !errors;
  }

let parse_feature_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf filename;
  try
    let f = Gherkin_parser.feature Gherkin_lexer.next_token lexbuf in
    close_in ic; f
  with e -> close_in ic; raise e

let run_feature_file t ~make_ctx filename =
  run_feature t ~make_ctx (parse_feature_file filename)

(** Integrate with Alcotest — each scenario becomes a test case. *)
let to_alcotest t ~make_ctx (feat : G.feature) =
  let cases =
    List.map
      (fun (sc : G.scenario) ->
        Alcotest.test_case sc.name `Quick (fun () ->
          match run_scenario t ~make_ctx sc with
          | Ok () -> ()
          | Error msg -> Alcotest.fail msg))
      feat.scenarios
  in
  (feat.name, cases)

let to_alcotest_from_file t ~make_ctx filename =
  to_alcotest t ~make_ctx (parse_feature_file filename)
