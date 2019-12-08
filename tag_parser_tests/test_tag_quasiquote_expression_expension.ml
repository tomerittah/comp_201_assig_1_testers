#use "topfind";;
#require "oUnit";;

#use "tag-parser.ml";;

open Tag_Parser;;
open OUnit2;;


let test_tag_quasiquote_expression_parser_0 text_ctxt = assert_equal (Var "a")
                                            (Tag_Parser.tag_parse_expression 
                                            (Pair (Symbol "quasiquote",
                                            Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)), Nil))))

let test_tag_quasiquote_expression_parser_1 text_ctxt = assert_equal (Applic (Var "cons",
                                        [Const (Sexpr (Symbol "a"));
                                        Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])]))
                                        (Tag_Parser.tag_parse_expression (Pair (Symbol "quasiquote",
                                        Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Nil))));;


let test_tag_quasiquote_expression_parser_2 text_ctxt = assert_equal (Applic (Var "cons",
                                [Var "a";
                                Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])]))
                                (Tag_Parser.tag_parse_expression (Pair (Symbol "quasiquote",
                                Pair
                                (Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),
                                    Pair (Symbol "b", Nil)),
                                Nil))));;


let test_tag_quasiquote_expression_parser_3 text_ctxt = assert_equal (Applic (Var "cons",
                                [Var "a"; Applic (Var "cons", [Var "b"; Const (Sexpr Nil)])]))

                            (Tag_Parser.tag_parse_expression (Pair (Symbol "quasiquote",
                                    Pair
                                    (Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),
                                        Pair (Pair (Symbol "unquote", Pair (Symbol "b", Nil)), Nil)),
                                    Nil))));;

let test_tag_quasiquote_expression_parser_4 text_ctxt = assert_equal (Applic (Var "cons",
                                    [Var "a"; Applic (Var "append", [Var "b"; Const (Sexpr Nil)])]))

                                (Tag_Parser.tag_parse_expression (Pair (Symbol "quasiquote",
                                Pair
                                (Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),
                                    Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),
                                Nil))));;

let test_tag_quasiquote_expression_parser_5 text_ctxt = assert_equal (Applic (Var "append",
                                            [Var "a";
                                            Applic (Var "append",
                                            [Var "h";
                                                Applic (Var "cons",
                                                [Applic (Var "foo", [Var "x"; Var "y"]);
                                                Applic (Var "cons",
                                                [Applic (Var "+", [Var "x"; Const (Sexpr (Number (Int 4)))]);
                                                    Applic (Var "cons", [Const (Sexpr (Symbol "q")); Const (Sexpr Nil)])])])])]))

                (Tag_Parser.tag_parse_expression (Pair (Symbol "quasiquote",
                                            Pair
                                            (Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),
                                                Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "h", Nil)),
                                                Pair
                                                (Pair (Symbol "unquote",
                                                    Pair (Pair (Symbol "foo", Pair (Symbol "x", Pair (Symbol "y", Nil))),
                                                    Nil)),
                                                Pair
                                                (Pair (Symbol "unquote",
                                                    Pair
                                                    (Pair (Symbol "+", Pair (Symbol "x", Pair (Number (Int 4), Nil))),
                                                    Nil)),
                                                Pair (Symbol "q", Nil))))),
                                            Nil))));;

let test_tag_quasiquote_expression_parser_6 text_ctxt = assert_equal (Applic (Var "cons",
                                            [Var "a";
                                            Applic (Var "cons",
                                            [Const (Sexpr (Symbol "sym"));
                                                Applic (Var "append",
                                                [Applic (Const (Sexpr (Number (Int 1))),
                                                [Const (Sexpr (Number (Int 2))); Const (Sexpr (Number (Int 3)))]);
                                                Const (Sexpr Nil)])])]))
                                        (Tag_Parser.tag_parse_expression
                                        (Pair (Symbol "quasiquote",
                                        Pair
                                        (Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),
                                            Pair (Symbol "sym",
                                            Pair
                                            (Pair (Symbol "unquote-splicing",
                                                Pair
                                                (Pair (Number (Int 1),
                                                Pair (Number (Int 2), Pair (Number (Int 3), Nil))),
                                                Nil)),
                                            Nil))),
                                        Nil))));;


let test_tag_quasiquote_expression_parser_7 test_ctxt = assert_equal (Applic (Var "cons",
                                [Const (Sexpr (Symbol "a"));
                                Applic (Var "cons",
                                [Const (Sexpr (Symbol "b"));
                                    Applic (Var "cons",
                                    [Const (Sexpr (Symbol "c"));
                                    Applic (Var "cons",
                                    [Var "d";
                                        Applic (Var "cons",
                                        [Const (Sexpr (Symbol "e"));
                                        Applic (Var "cons",
                                        [Const (Sexpr (Symbol "f"));
                                            Applic (Var "append",
                                            [Applic (Var "foo", [Var "x"; Var "y"]);
                                            Applic (Var "cons",
                                            [Var "g";
                                                Applic (Var "cons",
                                                [Const (Sexpr (Symbol "h"));
                                                Applic (Var "cons",
                                                [Const (Sexpr (Symbol "i"));
                                                    Applic (Var "cons",
                                                    [Const (Sexpr (Symbol "j")); Const (Sexpr Nil)])])])])])])])])])])]))

                    (Tag_Parser.tag_parse_expression (Pair (Symbol "quasiquote",
                                                Pair
                                                (Pair (Symbol "a",
                                                    Pair (Symbol "b",
                                                    Pair (Symbol "c",
                                                    Pair (Pair (Symbol "unquote", Pair (Symbol "d", Nil)),
                                                    Pair (Symbol "e",
                                                        Pair (Symbol "f",
                                                        Pair
                                                        (Pair (Symbol "unquote-splicing",
                                                            Pair
                                                            (Pair (Symbol "foo", Pair (Symbol "x", Pair (Symbol "y", Nil))),
                                                            Nil)),
                                                        Pair (Pair (Symbol "unquote", Pair (Symbol "g", Nil)),
                                                        Pair (Symbol "h", Pair (Symbol "i", Pair (Symbol "j", Nil))))))))))),
                                                Nil))));;

let test_tag_quasiquote_expression_parser_8 test_ctxt = assert_equal (Applic (Var "cons", [Var "a"; Var "b"]))
                            (Tag_Parser.tag_parse_expression   (Pair (Symbol "quasiquote",
                                                                Pair
                                                                (Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),
                                                                    Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil))),
                                                                Nil))));;

(* Name the test cases and group them together *)
let tag_quasiquote_expression_expension_tester_suite =
"tag_let_expression_parser_tester_suite">:::
 ["test_tag_quasiquote_expression_parser_0">:: test_tag_quasiquote_expression_parser_0;
  "test_tag_quasiquote_expression_parser_1">:: test_tag_quasiquote_expression_parser_1;
  "test_tag_quasiquote_expression_parser_2">:: test_tag_quasiquote_expression_parser_2;
  "test_tag_quasiquote_expression_parser_3">:: test_tag_quasiquote_expression_parser_3;
  "test_tag_quasiquote_expression_parser_4">:: test_tag_quasiquote_expression_parser_4;
  "test_tag_quasiquote_expression_parser_5">:: test_tag_quasiquote_expression_parser_5;
  "test_tag_quasiquote_expression_parser_6">:: test_tag_quasiquote_expression_parser_6;
  "test_tag_quasiquote_expression_parser_7">:: test_tag_quasiquote_expression_parser_7;
  "test_tag_quasiquote_expression_parser_8">:: test_tag_quasiquote_expression_parser_8;
  ]
;;

let () =
  run_test_tt_main tag_quasiquote_expression_expension_tester_suite
;;
