#use "topfind";;
#require "oUnit";;

#use "reader.ml";;

open Reader;;
open OUnit2;;

(* TESTER: test_list_parser.ml *)

let test_number_list test_ctxt = assert_equal (Pair (Number (Int 1), Pair (Number (Int 5), Nil)))
                                              (Reader.read_sexpr "(1 5)");;
let test_combined_list_1 test_ctxt = assert_equal (Pair (Number (Int 1), Pair (Symbol ("b"), Pair (Number (Int 5), Nil))))
                                              (Reader.read_sexpr "(1 b 5)");;
let test_nested_list_1 test_ctxt = assert_equal (Pair (Pair (Number (Int 1), (Pair (Pair (Symbol ("b"), Nil), Nil))), Pair (Number (Int 5), Nil)))
                                              (Reader.read_sexpr "((1 (b)) 5)");;

let test_empty_list test_ctxt = assert_equal (Nil) (Reader.read_sexpr "()");;
let test_comment_in_list_1 test_ctxt = assert_equal (Pair (String "this", Nil)) (Reader.read_sexpr "(\"this\" ; hahahahahaa \n)");;
let test_comment_in_list_2 test_ctxt = assert_equal ([Nil]) (Reader.read_sexprs "( ;sad\n )");;
let test_comment_in_list_3 test_ctxt = assert_equal (Nil) (Reader.read_sexpr "( ;sad\n )");;


let test_dotted_list_1 test_ctxt = assert_equal (Pair (Pair (Number (Int 1), Pair (Number (Int 5), Nil)), Number (Int 6)))
                                              (Reader.read_sexpr "((1 5) . 6)");;
let test_dotted_list_2 test_ctxt = assert_equal (Pair (Number (Float 1.2), Number (Int(3))))
                                              (Reader.read_sexpr "(1.2 . 3)");;
let test_dotted_list_3 test_ctxt = assert_equal (Pair (Symbol ("a"), Symbol ("b")))
                                              (Reader.read_sexpr "(a . b)");;

let test_dotted_list_nested_1 test_ctxt = assert_equal (Pair (Symbol ("a"), (Pair (Symbol ("b"), Symbol ("c")))))
                                              (Reader.read_sexpr "(a . (b . c))");;
let test_dotted_list_nested_2 test_ctxt = assert_equal (Pair (Symbol ("a"), (Pair (Symbol ("b"), Pair (Symbol ("c"), (Pair (Symbol ("d"), Symbol ("e"))))))))
                                              (Reader.read_sexpr "(a . (b . (c . (d . e))))");;

(* Name the test cases and group them together *)
let list_parser_tester_suite =
"list_parser_tester_suite">:::
 ["test_number_list">:: test_number_list;
  "test_combined_list_1">:: test_combined_list_1;
  "test_nested_list_1">:: test_nested_list_1;
  "test_empty_list">:: test_empty_list;
  "test_comment_in_list_1">:: test_comment_in_list_1;
  "test_comment_in_list_2">:: test_comment_in_list_2;
  "test_comment_in_list_3">:: test_comment_in_list_3;

  "test_dotted_list_1">:: test_dotted_list_1;
  "test_dotted_list_2">:: test_dotted_list_2;
  "test_dotted_list_3">:: test_dotted_list_3;

  "test_dotted_list_nested_1">:: test_dotted_list_nested_1;
  "test_dotted_list_nested_2">:: test_dotted_list_nested_2;
  ]
;;

let () =
  run_test_tt_main list_parser_tester_suite
;;
(* END TESTER: test_list_parser.ml *)
