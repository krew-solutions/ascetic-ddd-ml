open Ascetic_ddd.Result_ext

let test_let_star_ok () =
  let result =
    let* a = Ok 1 in
    let* b = Ok 2 in
    Ok (a + b)
  in
  Alcotest.(check (result int string)) "let* chains Ok" (Ok 3) result

let test_let_star_short_circuits () =
  let result =
    let* _ = Error "fail" in
    let* _ = Ok 2 in
    Ok 42
  in
  Alcotest.(check (result int string)) "let* short-circuits on Error"
    (Error "fail") result

let test_let_plus () =
  let result =
    let+ x = Ok 5 in
    x * 2
  in
  Alcotest.(check (result int string)) "let+ maps Ok" (Ok 10) result

let test_and_star () =
  let result =
    let* (a, b) = (and*) (Ok 1) (Ok 2) in
    Ok (a + b)
  in
  Alcotest.(check (result int string)) "and* combines Ok" (Ok 3) result

let test_and_star_first_error () =
  let result = (and*) (Ok 1) (Error "e") in
  Alcotest.(check (result (pair int int) string))
    "and* returns first Error" (Error "e") result

let test_traverse_all_ok () =
  let result = traverse (fun x -> Ok (x * 2)) [ 1; 2; 3 ] in
  Alcotest.(check (result (list int) string))
    "traverse all Ok" (Ok [ 2; 4; 6 ]) result

let test_traverse_short_circuits () =
  let calls = ref 0 in
  let result =
    traverse
      (fun x ->
        incr calls;
        if x = 2 then Error "bad" else Ok (x * 2))
      [ 1; 2; 3 ]
  in
  Alcotest.(check (result (list int) string))
    "traverse stops on Error" (Error "bad") result;
  Alcotest.(check int) "traverse short-circuits" 2 !calls

let test_sequence () =
  let result = sequence [ Ok 1; Ok 2; Ok 3 ] in
  Alcotest.(check (result (list int) string))
    "sequence collects" (Ok [ 1; 2; 3 ]) result

let test_sequence_error () =
  let result = sequence [ Ok 1; Error "e"; Ok 3 ] in
  Alcotest.(check (result (list int) string))
    "sequence first error" (Error "e") result

let test_map_error () =
  let result = map_error String.length (Error "abc") in
  Alcotest.(check (result int int)) "map_error" (Error 3) result

let test_or_else () =
  let result = or_else (Error "e") (fun () -> Ok 42) in
  Alcotest.(check (result int string)) "or_else fallback" (Ok 42) result

let test_or_else_ok () =
  let result = or_else (Ok 1) (fun () -> Ok 42) in
  Alcotest.(check (result int string)) "or_else keeps Ok" (Ok 1) result

let test_guard_true () =
  Alcotest.(check (result unit string))
    "guard true" (Ok ()) (guard true ~error:"e")

let test_guard_false () =
  Alcotest.(check (result unit string))
    "guard false" (Error "e") (guard false ~error:"e")

let test_of_option () =
  Alcotest.(check (result int string))
    "of_option Some" (Ok 1) (of_option ~error:"e" (Some 1));
  Alcotest.(check (result int string))
    "of_option None" (Error "e") (of_option ~error:"e" None)

let test_to_option () =
  Alcotest.(check (option int)) "to_option Ok" (Some 1) (to_option (Ok 1));
  Alcotest.(check (option int))
    "to_option Error" None
    (to_option (Error "e"))

let test_tap () =
  let side = ref 0 in
  let result = tap (fun x -> side := x) (Ok 42) in
  Alcotest.(check (result int string)) "tap returns value" (Ok 42) result;
  Alcotest.(check int) "tap side effect" 42 !side

let () =
  Alcotest.run "Result_ext"
    [
      ( "binding operators",
        [
          Alcotest.test_case "let* Ok" `Quick test_let_star_ok;
          Alcotest.test_case "let* Error" `Quick test_let_star_short_circuits;
          Alcotest.test_case "let+" `Quick test_let_plus;
          Alcotest.test_case "and* Ok" `Quick test_and_star;
          Alcotest.test_case "and* Error" `Quick test_and_star_first_error;
        ] );
      ( "combinators",
        [
          Alcotest.test_case "traverse Ok" `Quick test_traverse_all_ok;
          Alcotest.test_case "traverse Error" `Quick
            test_traverse_short_circuits;
          Alcotest.test_case "sequence Ok" `Quick test_sequence;
          Alcotest.test_case "sequence Error" `Quick test_sequence_error;
          Alcotest.test_case "map_error" `Quick test_map_error;
          Alcotest.test_case "or_else" `Quick test_or_else;
          Alcotest.test_case "or_else Ok" `Quick test_or_else_ok;
          Alcotest.test_case "guard true" `Quick test_guard_true;
          Alcotest.test_case "guard false" `Quick test_guard_false;
          Alcotest.test_case "of_option" `Quick test_of_option;
          Alcotest.test_case "to_option" `Quick test_to_option;
          Alcotest.test_case "tap" `Quick test_tap;
        ] );
    ]
