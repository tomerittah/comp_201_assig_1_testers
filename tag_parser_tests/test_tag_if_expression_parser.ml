#use "topfind";;
#require "oUnit";;

#use "tag-parser.ml";;

open Tag_Parser;;
open OUnit2;;

let test_tag_if_expression_parser_1 test_ctxt = assert_equal (If (Const (Sexpr (Bool true)), Const (Sexpr (Bool true)), Const (Sexpr (Bool false))))
  (Tag_Parser.tag_parse_expression (Pair (Symbol "if", Pair (Bool true, Pair (Bool true, Pair (Bool false, Nil))))));;

let test_tag_if_expression_parser_2 test_ctxt = assert_equal (If (Const (Sexpr (Bool true)), Const (Sexpr (Symbol "tomer!")),
                                                                Const (Sexpr (String "no!"))))
                                                (Tag_Parser.tag_parse_expression (Pair (Symbol "if",
                                                                                   Pair (Bool true,
                                                                                    Pair (Pair (Symbol "quote", Pair (Symbol "tomer!", Nil)),
                                                                                     Pair (String "no!", Nil))))))

let test_tag_if_expression_parser_3 test_ctxt = assert_equal (If (Const (Sexpr (Bool true)), Var "tomer!", Const (Sexpr (String "no!"))))
                                               (Tag_Parser.tag_parse_expression (Pair (Symbol "if",
                                                                                  Pair (Bool true,
                                                                                    Pair (Symbol "tomer!",
                                                                                      Pair (String "no!", Nil))))))

let test_tag_if_expression_parser_4 test_ctxt = assert_equal (If (Const (Sexpr (Bool true)), (Const (Sexpr (Bool true))), Const (Void)))
                                                 (Tag_Parser.tag_parse_expression (Pair (Symbol "if",
                                                                                    Pair (Bool true,
                                                                                      Pair (Bool true, Nil)))));;

let test_tag_if_expression_parser_5 test_ctxt = assert_equal (If (Const (Sexpr (String "hello")), Const (Sexpr (String "hello")),
                                                                  Const (Sexpr (String "bye"))))
                                               (Tag_Parser.tag_parse_expression (Pair (Symbol "if",
                                                                                  Pair (String "hello",
                                                                                    Pair (String "hello",
                                                                                      Pair (String "bye", Nil))))));;

let test_tag_if_expression_parser_6 test_ctxt = assert_equal (If (Or [Const (Sexpr (Bool true)); Const (Sexpr (Bool false))],
                                                                Const (Sexpr (Bool true)), Const (Sexpr (Bool false))))
                                               (Tag_Parser.tag_parse_expression (Pair (Symbol "if",
                                                     Pair (Pair (Symbol "or", Pair (Bool true, Pair (Bool false, Nil))),
                                                      Pair (Bool true, Pair (Bool false, Nil))))    ));;

let test_tag_if_expression_parser_7 test_ctxt = assert_equal (Const (Sexpr (Bool false)))
                                                (Tag_Parser.tag_parse_expression (Pair (Symbol "or", Nil)))

let test_tag_if_expression_parser_8 test_ctxt = assert_equal (Const (Sexpr (Number (Int 7))))
                                                (Tag_Parser.tag_parse_expression (Pair (Symbol "or", Pair (Number (Int 7), Nil))))


(* Name the test cases and group them together *)
let tag_if_expression_parser_tester_suite =
"tag_if_expression_parser_tester_suite">:::
 ["test_tag_if_expression_parser_1">:: test_tag_if_expression_parser_1;
  "test_tag_if_expression_parser_2">:: test_tag_if_expression_parser_2;
  "test_tag_if_expression_parser_3">:: test_tag_if_expression_parser_3;
  "test_tag_if_expression_parser_4">:: test_tag_if_expression_parser_4;
  "test_tag_if_expression_parser_5">:: test_tag_if_expression_parser_5;
  "test_tag_if_expression_parser_6">:: test_tag_if_expression_parser_6;
  "test_tag_if_expression_parser_7">:: test_tag_if_expression_parser_7;
  "test_tag_if_expression_parser_8">:: test_tag_if_expression_parser_8;
  ]
;;

let () =
  run_test_tt_main tag_if_expression_parser_tester_suite
;;
