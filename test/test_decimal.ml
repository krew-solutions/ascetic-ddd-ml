module D = Ascetic_ddd.Decimal

let decimal = Alcotest.testable D.pp D.equal

let test_of_int () =
  let d = D.of_int 42 in
  Alcotest.(check string) "of_int 42" "42" (D.to_string d)

let test_of_int_negative () =
  let d = D.of_int (-3) in
  Alcotest.(check string) "of_int -3" "-3" (D.to_string d)

let test_of_string () =
  let d = D.of_string_exn "1.5" in
  Alcotest.(check string) "of_string 1.5" "1.5" (D.to_string d)

let test_of_string_negative () =
  let d = D.of_string_exn "-3.14" in
  Alcotest.(check string) "of_string -3.14" "-3.14" (D.to_string d)

let test_of_string_no_frac () =
  let d = D.of_string_exn "7" in
  Alcotest.(check string) "of_string 7" "7" (D.to_string d)

let test_of_string_invalid () =
  Alcotest.(check (option decimal))
    "of_string invalid" None (D.of_string "abc")

let test_add () =
  let a = D.of_string_exn "1.5" in
  let b = D.of_string_exn "2.3" in
  Alcotest.(check string) "1.5 + 2.3" "3.8" (D.to_string (D.add a b))

let test_sub () =
  let a = D.of_string_exn "5.0" in
  let b = D.of_string_exn "2.3" in
  Alcotest.(check string) "5 - 2.3" "2.7" (D.to_string (D.sub a b))

let test_mul () =
  let a = D.of_string_exn "2.5" in
  let b = D.of_string_exn "4.0" in
  Alcotest.(check string) "2.5 * 4" "10" (D.to_string (D.mul a b))

let test_div () =
  let a = D.of_int 10 in
  let b = D.of_int 4 in
  Alcotest.(check string) "10 / 4" "2.5" (D.to_string (D.div a b))

let test_div_by_zero () =
  Alcotest.check_raises "div by zero" Division_by_zero (fun () ->
      ignore (D.div (D.of_int 1) D.zero))

let test_zero () =
  Alcotest.(check bool) "zero is zero" true (D.is_zero D.zero)

let test_one () =
  Alcotest.(check string) "one" "1" (D.to_string D.one)

let test_neg () =
  let d = D.of_int 5 in
  Alcotest.(check string) "neg 5" "-5" (D.to_string (D.neg d))

let test_abs () =
  let d = D.of_int (-7) in
  Alcotest.(check string) "abs -7" "7" (D.to_string (D.abs d))

let test_clamp_non_negative () =
  Alcotest.(check decimal)
    "clamp positive" (D.of_int 5)
    (D.clamp_non_negative (D.of_int 5));
  Alcotest.(check decimal)
    "clamp negative" D.zero
    (D.clamp_non_negative (D.of_int (-3)))

let test_comparison () =
  let a = D.of_int 3 in
  let b = D.of_int 5 in
  Alcotest.(check bool) "3 < 5" true (D.compare a b < 0);
  Alcotest.(check bool) "5 > 3" true (D.compare b a > 0);
  Alcotest.(check bool) "3 = 3" true (D.equal a a)

let test_predicates () =
  Alcotest.(check bool) "positive" true (D.is_positive (D.of_int 1));
  Alcotest.(check bool) "negative" true (D.is_negative (D.of_int (-1)));
  Alcotest.(check bool)
    "non_negative 0" true
    (D.is_non_negative D.zero);
  Alcotest.(check bool)
    "non_negative 1" true
    (D.is_non_negative (D.of_int 1))

let test_scale () =
  let d = D.of_string_exn "1.5" in
  Alcotest.(check string) "1.5 * 3" "4.5" (D.to_string (D.scale d 3))

let test_roundtrip_raw () =
  let d = D.of_string_exn "3.14" in
  let raw = D.to_raw d in
  let d2 = D.of_raw raw in
  Alcotest.(check decimal) "raw roundtrip" d d2

let test_to_float () =
  let d = D.of_string_exn "2.5" in
  Alcotest.(check (float 0.0001)) "to_float" 2.5 (D.to_float d)

let test_min_max () =
  let a = D.of_int 3 in
  let b = D.of_int 7 in
  Alcotest.(check decimal) "min" a (D.min a b);
  Alcotest.(check decimal) "max" b (D.max a b)

let () =
  Alcotest.run "Decimal"
    [
      ( "construction",
        [
          Alcotest.test_case "of_int" `Quick test_of_int;
          Alcotest.test_case "of_int negative" `Quick test_of_int_negative;
          Alcotest.test_case "of_string" `Quick test_of_string;
          Alcotest.test_case "of_string negative" `Quick
            test_of_string_negative;
          Alcotest.test_case "of_string no frac" `Quick test_of_string_no_frac;
          Alcotest.test_case "of_string invalid" `Quick test_of_string_invalid;
        ] );
      ( "arithmetic",
        [
          Alcotest.test_case "add" `Quick test_add;
          Alcotest.test_case "sub" `Quick test_sub;
          Alcotest.test_case "mul" `Quick test_mul;
          Alcotest.test_case "div" `Quick test_div;
          Alcotest.test_case "div by zero" `Quick test_div_by_zero;
          Alcotest.test_case "neg" `Quick test_neg;
          Alcotest.test_case "abs" `Quick test_abs;
          Alcotest.test_case "clamp" `Quick test_clamp_non_negative;
          Alcotest.test_case "scale" `Quick test_scale;
          Alcotest.test_case "min/max" `Quick test_min_max;
        ] );
      ( "conversion",
        [
          Alcotest.test_case "zero" `Quick test_zero;
          Alcotest.test_case "one" `Quick test_one;
          Alcotest.test_case "comparison" `Quick test_comparison;
          Alcotest.test_case "predicates" `Quick test_predicates;
          Alcotest.test_case "raw roundtrip" `Quick test_roundtrip_raw;
          Alcotest.test_case "to_float" `Quick test_to_float;
        ] );
    ]
