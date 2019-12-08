#use "topfind";;
#require "oUnit";;

#use "reader.ml";;

open Reader;;
open OUnit2;;

(* TESTER: test_scientific_parser.ml *)
let test_positive_int_signed1 test_ctxt = assert_equal (Number (Float (120.))) (Reader.read_sexpr "+12e1  ");;
let test_positive_int_signed2 test_ctxt = assert_equal (Number (Float (10.))) (Reader.read_sexpr "+1e1  ");;
let test_positive_int_signed3 test_ctxt = assert_equal (Number (Float (10.))) (Reader.read_sexpr "1E1 ");;
let test_positive_int_signed4 test_ctxt = assert_equal (Number (Float (1230.))) (Reader.read_sexpr "    +000000012.3E00000002");;


let test_positive_int_second_signed1 test_ctxt = assert_equal (Number (Float (10.))) (Reader.read_sexpr "1E+1 ");;
let test_positive_int_second_signed2 test_ctxt = assert_equal (Number (Float (0.1))) (Reader.read_sexpr "1E-1 ");;
let test_positive_int_second_signed3 test_ctxt = assert_equal (Number (Float (13.4))) (Reader.read_sexpr "0000134E-1 ");;
let test_positive_int_second_signed4 test_ctxt = assert_equal (Number (Float 7200000000.)) (Reader.read_sexpr "00072E+00008 ");;
let test_positive_float_second_signed5 test_ctxt = assert_equal (Number (Float 3140000000.)) (Reader.read_sexpr "  3.14e+9 ");;



let test_positive_int_unsigned test_ctxt = assert_equal (Number (Float (100.))) (Reader.read_sexpr " 10e1  ");;


let test_negative_float_second_signed test_ctxt = assert_equal (Number (Float 0.)) (Reader.read_sexpr "  3.14E-512 ");;


let test_negative_float_signed_both test_ctxt = assert_equal (Number (Float (-0.05))) (Reader.read_sexpr "    -5.000000000e-2  ");;


let test_assert_raises_1 test_ctxt = assert_raises X_no_match (fun _ -> (Reader.read_sexpr "    -5.000000000eff-2  "));;
let test_assert_raises_2 test_ctxt = assert_raises X_no_match (fun _ -> (Reader.read_sexpr "    -540300@e-2  "));;
(* test 3 raises no match cause of @ doesn't exist in any symbol. *)
let test_assert_raises_3 test_ctxt = assert_raises X_no_match (fun _ -> (Reader.read_sexpr "5.eff-2"));;
let test_assert_raises_4 test_ctxt = assert_raises X_no_match (fun _ -> (Reader.read_sexpr "6.2ea"));;
let test_assert_raises_5 test_ctxt = assert_raises X_no_match (fun _ -> (Reader.read_sexpr "4e.5"));;


(* Name the test cases and group them together *)
let scientific_parser_tester_suite =
  "scientific_parser_tester_suite">:::
    ["test_positive_int_signed1">:: test_positive_int_signed1;
     "test_positive_int_signed2">:: test_positive_int_signed2;
     "test_positive_int_signed3">:: test_positive_int_signed3;
     "test_positive_int_signed4">:: test_positive_int_signed4;

     "test_positive_int_second_signed1">:: test_positive_int_second_signed1;
     "test_positive_int_second_signed2">:: test_positive_int_second_signed2;
     "test_positive_int_second_signed3">:: test_positive_int_second_signed3;
     "test_positive_int_second_signed4">:: test_positive_int_second_signed4;
     "test_positive_float_second_signed5">:: test_positive_float_second_signed5;

     "test_positive_int_unsigned">:: test_positive_int_unsigned;

     "test_negative_float_second_signed">:: test_negative_float_second_signed;

     "test_negative_float_signed_both">:: test_negative_float_signed_both;

     "test_assert_raises_1">:: test_assert_raises_1;
     "test_assert_raises_2">:: test_assert_raises_2;
     "test_assert_raises_3">:: test_assert_raises_3;
     "test_assert_raises_4">:: test_assert_raises_4;
     "test_assert_raises_5">:: test_assert_raises_5;
    ];;

let () =
  run_test_tt_main scientific_parser_tester_suite
;;
(* END TESTER: test_scientific_parser.ml *)
