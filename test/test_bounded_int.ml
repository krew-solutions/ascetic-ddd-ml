module Score = Ascetic_ddd.Bounded_int.Make (struct
  let min_value = 0
  let max_value = 100
  let name = "Score"
end)

let score = Alcotest.testable Score.pp Score.equal

let test_of_int_valid () =
  let s = Score.of_int 50 in
  Alcotest.(check (option score))
    "of_int 50" (Some (Score.of_int_exn 50)) s

let test_of_int_min () =
  let s = Score.of_int 0 in
  Alcotest.(check (option score))
    "of_int 0 (min)" (Some (Score.of_int_exn 0)) s

let test_of_int_max () =
  let s = Score.of_int 100 in
  Alcotest.(check (option score))
    "of_int 100 (max)" (Some (Score.of_int_exn 100)) s

let test_of_int_below_min () =
  Alcotest.(check (option score))
    "of_int -1" None (Score.of_int (-1))

let test_of_int_above_max () =
  Alcotest.(check (option score))
    "of_int 101" None (Score.of_int 101)

let test_of_int_exn_invalid () =
  Alcotest.check_raises "of_int_exn -1"
    (Invalid_argument "Score.of_int_exn: -1 not in [0, 100]")
    (fun () -> ignore (Score.of_int_exn (-1)))

let test_of_int_clamped_below () =
  let s = Score.of_int_clamped (-10) in
  Alcotest.(check int) "clamped below" 0 (Score.to_int s)

let test_of_int_clamped_above () =
  let s = Score.of_int_clamped 200 in
  Alcotest.(check int) "clamped above" 100 (Score.to_int s)

let test_of_int_clamped_in_range () =
  let s = Score.of_int_clamped 42 in
  Alcotest.(check int) "clamped in range" 42 (Score.to_int s)

let test_to_int () =
  let s = Score.of_int_exn 73 in
  Alcotest.(check int) "to_int" 73 (Score.to_int s)

let test_equal () =
  let a = Score.of_int_exn 50 in
  let b = Score.of_int_exn 50 in
  Alcotest.(check bool) "equal" true (Score.equal a b)

let test_compare () =
  let a = Score.of_int_exn 30 in
  let b = Score.of_int_exn 70 in
  Alcotest.(check bool) "compare <" true (Score.compare a b < 0)

let test_zero () =
  match Score.zero with
  | Some z -> Alcotest.(check int) "zero" 0 (Score.to_int z)
  | None -> Alcotest.fail "zero should be Some for [0, 100]"

let test_show () =
  let s = Score.of_int_exn 42 in
  Alcotest.(check string) "show" "Score(42)" (Score.show s)

(* Test with a range that doesn't include 0 *)
module Positive = Ascetic_ddd.Bounded_int.Make (struct
  let min_value = 1
  let max_value = 10
  let name = "Positive"
end)

let test_zero_out_of_range () =
  Alcotest.(check (option (Alcotest.testable Positive.pp Positive.equal)))
    "zero out of range" None Positive.zero

let () =
  Alcotest.run "Bounded_int"
    [
      ( "construction",
        [
          Alcotest.test_case "valid" `Quick test_of_int_valid;
          Alcotest.test_case "min boundary" `Quick test_of_int_min;
          Alcotest.test_case "max boundary" `Quick test_of_int_max;
          Alcotest.test_case "below min" `Quick test_of_int_below_min;
          Alcotest.test_case "above max" `Quick test_of_int_above_max;
          Alcotest.test_case "exn invalid" `Quick test_of_int_exn_invalid;
          Alcotest.test_case "clamped below" `Quick test_of_int_clamped_below;
          Alcotest.test_case "clamped above" `Quick test_of_int_clamped_above;
          Alcotest.test_case "clamped in range" `Quick
            test_of_int_clamped_in_range;
        ] );
      ( "operations",
        [
          Alcotest.test_case "to_int" `Quick test_to_int;
          Alcotest.test_case "equal" `Quick test_equal;
          Alcotest.test_case "compare" `Quick test_compare;
          Alcotest.test_case "zero" `Quick test_zero;
          Alcotest.test_case "zero out of range" `Quick test_zero_out_of_range;
          Alcotest.test_case "show" `Quick test_show;
        ] );
    ]
