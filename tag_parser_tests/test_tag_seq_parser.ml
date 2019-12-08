#use "topfind";;
#require "oUnit";;

#use "tag-parser.ml";;

open Tag_Parser;;
open OUnit2;;

let test_tag_seq_expression_parser_1 test_ctxt = assert_equal (Const(Void))
  (Tag_Parser.tag_parse_expression (Pair(Symbol "begin", Nil)));;

let test_tag_seq_expression_parser_2 test_ctxt = assert_equal (Var "a")
  (Tag_Parser.tag_parse_expression (Pair(Symbol "begin", Pair(Symbol "a", Nil))));;

let test_tag_seq_expression_parser_3 test_ctxt = assert_equal (Const (Sexpr (Bool true)))
  (Tag_Parser.tag_parse_expression (Pair(Symbol "begin", Pair(Bool true, Nil))));;

(* (begin (if #t #t #f) #t) *)
let test_tag_seq_expression_parser_4 test_ctxt = assert_equal (Seq [If (Const (Sexpr (Bool true)),
                                                                         Const (Sexpr (Bool true)),
                                                                          Const (Sexpr (Bool false)));
                                                                        Const (Sexpr (Bool true))])
  (Tag_Parser.tag_parse_expression (Pair (Symbol "begin",
                                     Pair
                                      (Pair (Symbol "if",
                                        Pair (Bool true, Pair (Bool true, Pair (Bool false, Nil)))),
                                      Pair (Bool true, Nil)))));;


(* Name the test cases and group them together *)
let tag_seq_expression_parser_tester_suite =
"tag_lambda_expression_parser_tester_suite">:::
 ["test_tag_seq_expression_parser_1">:: test_tag_seq_expression_parser_1;
  "test_tag_seq_expression_parser_2">:: test_tag_seq_expression_parser_2;
  "test_tag_seq_expression_parser_3">:: test_tag_seq_expression_parser_3;
  "test_tag_seq_expression_parser_4">:: test_tag_seq_expression_parser_4;
  ]
;;

let () =
  run_test_tt_main tag_seq_expression_parser_tester_suite
;;
