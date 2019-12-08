#use "topfind";;
#require "oUnit";;

#use "tag-parser.ml";;

open Tag_Parser;;
open OUnit2;;

let test_tag_unreserved_variable_parser_1 test_ctxt = assert_equal (Var("tomer"))
  (Tag_Parser.tag_parse_expression (Symbol ("tomer")));;

(* Name the test cases and group them together *)
let tag_unreserved_variable_parser_tester_suite =
"tag_unreserved_variable_parser_tester_suite">:::
 ["test_tag_unreserved_variable_parser_1">:: test_tag_unreserved_variable_parser_1;
  ]
;;

let () =
  run_test_tt_main tag_unreserved_variable_parser_tester_suite
;;
