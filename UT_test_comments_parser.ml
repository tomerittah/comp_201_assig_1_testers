#use "topfind";;
#require "oUnit";;

#use "reader.ml";;

open Reader;;
open OUnit2;;

(* TESTER: test_comments_parser.ml *)
let test_comment0 test_ctxt = assert_equal (Symbol "BB") (Reader.read_sexpr ";Ab\r\nBB");;
let test_comment1 test_ctxt = assert_raises X_no_match (fun _ -> (Reader.read_sexpr ";Ab\r\n"));;

let comments_parser_tester_suite =
"comments_parser_tester_suite">:::
 ["test_comment0">:: test_comment0;
  "test_comment1">:: test_comment1;
  ]
;;

let () =
  run_test_tt_main comments_parser_tester_suite
;;
(* END TESTER: test_comments_parser.ml *)
