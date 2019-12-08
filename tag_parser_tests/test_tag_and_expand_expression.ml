#use "topfind";;
#require "oUnit";;

#use "tag-parser.ml";;

open Tag_Parser;;
open OUnit2;;


let test_tag_and_expression_parser_0 text_ctxt = assert_equal (Const (Sexpr (Bool true)))
                        (Tag_Parser.tag_parse_expression (Pair (Symbol "and", Nil)));;

let test_tag_and_expression_parser_1 text_ctxt = assert_equal (Const (Sexpr (String "hey")))
                        (Tag_Parser.tag_parse_expression (Pair (Symbol "and", Pair (String "hey", Nil))));;

let test_tag_and_expression_parser_2 text_ctxt = assert_equal (If (Const (Sexpr (String "hey")), Const (Sexpr (Number (Int 42))),
                                                                                Const (Sexpr (Bool false))))
                        (Tag_Parser.tag_parse_expression (Pair (Symbol "and", Pair (String "hey", Pair (Number (Int 42), Nil)))));;


let test_tag_and_expression_parser_3 text_ctxt = assert_equal (If (Const (Sexpr (String "hey")),
                                        If (Const (Sexpr (Number (Int 42))),
                                        If (Var "n",
                                        If (Var "np",
                                            If (Var "npm", If (Var "npm", Var "install", Const (Sexpr (Bool false))),
                                            Const (Sexpr (Bool false))),
                                            Const (Sexpr (Bool false))),
                                        Const (Sexpr (Bool false))),
                                        Const (Sexpr (Bool false))),
                                        Const (Sexpr (Bool false))))

                                (Tag_Parser.tag_parse_expression (Pair (Symbol "and",
                                            Pair (String "hey",
                                            Pair (Number (Int 42),
                                            Pair (Symbol "n",
                                                Pair (Symbol "np",
                                                Pair (Symbol "npm", Pair (Symbol "npm", Pair (Symbol "install", Nil))))))))));;

let test_tag_and_expression_parser_4 text_ctxt = assert_equal (If (Const (Sexpr (Pair (Symbol "ya!", Nil))),
                                                    If (Var "a",
                                                    If (Var "hezi",
                                                    If (Var "moshe",
                                                        If (Const (Sexpr (Symbol "moshe")),
                                                        If (Const (Sexpr (Bool true)),
                                                        If (Const (Sexpr (Number (Int 42))),
                                                        If (Var "again", Var "moshe", Const (Sexpr (Bool false))),
                                                        Const (Sexpr (Bool false))),
                                                        Const (Sexpr (Bool false))),
                                                        Const (Sexpr (Bool false))),
                                                        Const (Sexpr (Bool false))),
                                                    Const (Sexpr (Bool false))),
                                                    Const (Sexpr (Bool false))),
                                                    Const (Sexpr (Bool false))))    
        (Tag_Parser.tag_parse_expression (Pair (Symbol "and",
                                        Pair (Pair (Symbol "quote", Pair (Pair (Symbol "ya!", Nil), Nil)),
                                        Pair
                                        (Pair (Symbol "quasiquote",
                                            Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)), Nil)),
                                        Pair (Symbol "hezi",
                                            Pair (Symbol "moshe",
                                            Pair (Pair (Symbol "quote", Pair (Symbol "moshe", Nil)),
                                            Pair (Bool true,
                                            Pair (Number (Int 42),
                                                Pair (Symbol "again", Pair (Symbol "moshe", Nil))))))))))));;




(* Name the test cases and group them together *)
let tag_and_expression_expension_tester_suite =
"tag_and_expression_parser_tester_suite">:::
 ["test_tag_and_expression_parser_0">:: test_tag_and_expression_parser_0;
  "test_tag_and_expression_parser_1">:: test_tag_and_expression_parser_1;
  "test_tag_and_expression_parser_2">:: test_tag_and_expression_parser_2;
  "test_tag_and_expression_parser_3">:: test_tag_and_expression_parser_3;
  "test_tag_and_expression_parser_4">:: test_tag_and_expression_parser_4;
  ]
;;

let () =
  run_test_tt_main tag_and_expression_expension_tester_suite
;;