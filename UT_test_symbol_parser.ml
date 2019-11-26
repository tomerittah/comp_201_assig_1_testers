#use "topfind";;
#require "oUnit";;

#use "reader.ml";;

open Reader;;
open OUnit2;;

(* TESTER: test_symbol_parser.ml *)
let test_symbol_1 test_ctxt = assert_equal (Symbol "!0$aa!1") (Reader.read_sexpr "!0$aA!1");;
let test_symbol_2 test_ctxt = assert_equal (Symbol "zzaa!:=9") (Reader.read_sexpr "zZaA!:=9");;
let test_symbol_3 test_ctxt = assert_equal (Symbol "123a") (Reader.read_sexpr "123a");;
let test_symbol_3 test_ctxt = assert_equal (Symbol "123a") (Reader.read_sexpr "123A");;

let test_symbol_space_left test_ctxt = assert_equal (Symbol "!0$aa!1") (Reader.read_sexpr "!0$aA!1");;
let test_symbol_space_right test_ctxt = assert_equal (Symbol "!0$aa!1") (Reader.read_sexpr "!0$aA!1");;
let test_symbol_spaces_both_sides test_ctxt = assert_equal (Symbol "!0$aa!1") (Reader.read_sexpr "!0$aA!1");;

let test_symbol_raise_1 test_ctxt = assert_raises X_no_match (fun _ -> (Reader.read_sexpr "!0$aA!1@2"));;


(* Name the test cases and group them together *)
let symbol_parser_tester_suite =
"bool_parser_tester_suite">:::
 ["test_symbol_1">:: test_symbol_1;
  "test_symbol_2">:: test_symbol_2;
  "test_symbol_3">:: test_symbol_3;

  "test_symbol_space_left">:: test_symbol_space_left;
  "test_symbol_space_right">:: test_symbol_space_right;
  "test_symbol_spaces_both_sides">:: test_symbol_spaces_both_sides;

  "test_symbol_raise_1">:: test_symbol_raise_1;
  ]
;;

let () =
  run_test_tt_main symbol_parser_tester_suite
;;
(* END TESTER: test_symbol_parser.ml *)
