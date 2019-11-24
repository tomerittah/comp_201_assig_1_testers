#use "topfind";;
#require "oUnit";;

#use "reader.ml";;

open Reader;;
open OUnit2;;

(* TESTER: test_bool_parser.ml *)
let test_true_lower test_ctxt = assert_equal (Bool true) (Reader.read_sexpr "#t");;
let test_true_upper test_ctxt = assert_equal (Bool true) (Reader.read_sexpr "#T");;
let test_false_lower test_ctxt = assert_equal (Bool false) (Reader.read_sexpr "#f");;
let test_false_upper test_ctxt = assert_equal (Bool false) (Reader.read_sexpr "#F");;

let test_true_lower_whitespace_both_sides test_ctxt = assert_equal (Bool true)
  (Reader.read_sexpr "  #t ");;
let test_true_lower_whitespace_right_side test_ctxt = assert_equal (Bool true)
  (Reader.read_sexpr "#t   ");;
let test_true_lower_whitespace_left_side test_ctxt = assert_equal (Bool true)
  (Reader.read_sexpr "   #t");;

(* Name the test cases and group them together *)
let bool_parser_tester_suite =
"bool_parser_tester_suite">:::
 ["test_true_lower">:: test_true_lower;
  "test_true_upper">:: test_true_upper;
  "test_false_lower">:: test_false_lower;
  "test_false_upper">:: test_false_upper;
  "test_true_lower_whitespace_both_sides">:: test_true_lower_whitespace_both_sides;
  "test_true_lower_whitespace_right_side">:: test_true_lower_whitespace_right_side;
  "test_true_lower_whitespace_left_side">:: test_true_lower_whitespace_left_side;
  ]
;;

let () =
  run_test_tt_main bool_parser_tester_suite
;;
(* END TESTER: test_bool_parser.ml *)
