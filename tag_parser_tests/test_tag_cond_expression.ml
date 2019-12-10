#use "topfind";;
#require "oUnit";;

#use "tag-parser.ml";;

open Tag_Parser;;
open OUnit2;;

let test_tag_cond_expression_parser_0 text_ctxt = assert_equal (If (Const (Sexpr (Bool true)),
                                          Seq [Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 2)))],
                                          Const Void))

(Tag_Parser.tag_parse_expression (Pair (Symbol "cond",
                                    Pair
                                    (Pair (Pair (Symbol "and", Nil),
                                       Pair (Number (Int 1), Pair (Number (Int 2), Nil))),
                                    Nil))));;


let test_tag_cond_expression_parser_1 text_ctxt = assert_equal (If
                                      (If (Const (Sexpr (Number (Int 7))), Const (Sexpr (Number (Int 8))),
                                        Const (Sexpr (Bool false))),
                                      Seq [Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 2)))],
                                      If (Or [Const (Sexpr (Number (Int 3))); Const (Sexpr (Number (Int 4)))],
                                        Seq [Const (Sexpr (Number (Int 3))); Const (Sexpr (Number (Int 4)))],
                                        Const Void)))


(Tag_Parser.tag_parse_expression (Pair (Symbol "cond",
                                    Pair
                                      (Pair
                                        (Pair (Symbol "and", Pair (Number (Int 7), Pair (Number (Int 8), Nil))),
                                        Pair (Number (Int 1), Pair (Number (Int 2), Nil))),
                                      Pair
                                      (Pair
                                        (Pair (Symbol "or", Pair (Number (Int 3), Pair (Number (Int 4), Nil))),
                                        Pair (Number (Int 3), Pair (Number (Int 4), Nil))),
                                      Nil)))));;


let test_tag_cond_expression_parser_2 text_ctxt = assert_equal (If
                                          (If (Const (Sexpr (Number (Int 7))), Const (Sexpr (Number (Int 8))),
                                            Const (Sexpr (Bool false))),
                                          Seq [Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 2)))],
                                          If (Or [Const (Sexpr (Number (Int 3))); Const (Sexpr (Number (Int 4)))],
                                            Seq [Const (Sexpr (Number (Int 3))); Const (Sexpr (Number (Int 4)))],
                                            If
                                            (If (Const (Sexpr (Number (Int 5))), Const (Sexpr (Number (Int 6))),
                                              Const (Sexpr (Bool false))),
                                            Seq [Const (Sexpr (Number (Int 5))); Const (Sexpr (Number (Int 6)))],
                                            Const Void))))


(Tag_Parser.tag_parse_expression (Pair (Symbol "cond",
                                          Pair
                                            (Pair
                                              (Pair (Symbol "and", Pair (Number (Int 7), Pair (Number (Int 8), Nil))),
                                              Pair (Number (Int 1), Pair (Number (Int 2), Nil))),
                                            Pair
                                            (Pair
                                              (Pair (Symbol "or", Pair (Number (Int 3), Pair (Number (Int 4), Nil))),
                                              Pair (Number (Int 3), Pair (Number (Int 4), Nil))),
                                            Pair
                                              (Pair
                                                (Pair (Symbol "and", Pair (Number (Int 5), Pair (Number (Int 6), Nil))),
                                                Pair (Number (Int 5), Pair (Number (Int 6), Nil))),
                                              Nil))))));;



let test_tag_cond_expression_parser_3 text_ctxt = assert_equal (If
                                                  (If (Const (Sexpr (Number (Int 7))), Const (Sexpr (Number (Int 8))),
                                                    Const (Sexpr (Bool false))),
                                                  Seq [Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 2)))],
                                                  If (Or [Const (Sexpr (Number (Int 3))); Const (Sexpr (Number (Int 4)))],
                                                    Seq [Const (Sexpr (Number (Int 3))); Const (Sexpr (Number (Int 4)))],
                                                    If
                                                    (If (Const (Sexpr (Number (Int 5))), Const (Sexpr (Number (Int 6))),
                                                      Const (Sexpr (Bool false))),
                                                    Seq [Const (Sexpr (Number (Int 5))); Const (Sexpr (Number (Int 6)))],
                                                    If
                                                      (If (Const (Sexpr (Number (Int 7))), Const (Sexpr (Number (Int 8))),
                                                        Const (Sexpr (Bool false))),
                                                      Seq [Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 2)))],
                                                      Const Void)))))


(Tag_Parser.tag_parse_expression (Pair (Symbol "cond",
                                      Pair
                                        (Pair
                                          (Pair (Symbol "and", Pair (Number (Int 7), Pair (Number (Int 8), Nil))),
                                          Pair (Number (Int 1), Pair (Number (Int 2), Nil))),
                                        Pair
                                        (Pair
                                          (Pair (Symbol "or", Pair (Number (Int 3), Pair (Number (Int 4), Nil))),
                                          Pair (Number (Int 3), Pair (Number (Int 4), Nil))),
                                        Pair
                                          (Pair
                                            (Pair (Symbol "and", Pair (Number (Int 5), Pair (Number (Int 6), Nil))),
                                            Pair (Number (Int 5), Pair (Number (Int 6), Nil))),
                                          Pair
                                          (Pair
                                            (Pair (Symbol "and",
                                              Pair (Number (Int 7), Pair (Number (Int 8), Nil))),
                                            Pair (Number (Int 1), Pair (Number (Int 2), Nil))),
                                          Nil)))))));;

let test_tag_cond_expression_parser_4 text_ctxt = assert_equal (Applic
                                                (LambdaSimple (["value"; "f"],
                                                  If (Var "value", Applic (Applic (Var "f", []), [Var "value"]), Const Void)),
                                                [Applic (Var "h?", [Var "x"]);
                                                  LambdaSimple ([], Applic (Var "p", [Var "q"]))]))
          (Tag_Parser.tag_parse_expression (Pair (Symbol "cond",
                                    Pair
                                      (Pair (Pair (Symbol "h?", Pair (Symbol "x", Nil)),
                                        Pair (Symbol "=>", Pair (Pair (Symbol "p", Pair (Symbol "q", Nil)), Nil))),
                                      Nil))));;


let test_tag_cond_expression_parser_5 text_ctxt = assert_equal (If (Applic (Var "zero?", [Var "n"]),
                                            Seq [Applic (Var "f", [Var "x"]); Applic (Var "g", [Var "y"])],
                                            Applic
                                              (LambdaSimple (["value"; "f"; "rest"],
                                                If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),
                                                Applic (Var "rest", []))),
                                              [Applic (Var "h?", [Var "x"]);
                                              LambdaSimple ([], Applic (Var "p", [Var "q"]));
                                              LambdaSimple ([],
                                                Seq [Applic (Var "h", [Var "x"; Var "y"]); Applic (Var "g", [Var "x"])])])))
            (Tag_Parser.tag_parse_expression (Pair (Symbol "cond",
                                              Pair
                                                (Pair (Pair (Symbol "zero?", Pair (Symbol "n", Nil)),
                                                  Pair (Pair (Symbol "f", Pair (Symbol "x", Nil)),
                                                  Pair (Pair (Symbol "g", Pair (Symbol "y", Nil)), Nil))),
                                                Pair
                                                (Pair (Pair (Symbol "h?", Pair (Symbol "x", Nil)),
                                                  Pair (Symbol "=>",
                                                    Pair (Pair (Symbol "p", Pair (Symbol "q", Nil)), Nil))),
                                                Pair
                                                  (Pair (Symbol "else",
                                                    Pair (Pair (Symbol "h", Pair (Symbol "x", Pair (Symbol "y", Nil))),
                                                    Pair (Pair (Symbol "g", Pair (Symbol "x", Nil)), Nil))),
                                                  Pair
                                                  (Pair (Pair (Symbol "q?", Pair (Symbol "y", Nil)),
                                                    Pair (Pair (Symbol "p", Pair (Symbol "x", Nil)),
                                                      Pair (Pair (Symbol "q", Pair (Symbol "y", Nil)), Nil))),
                                                  Nil)))))));;


let test_tag_cond_expression_parser_6 text_ctxt = assert_equal (If (Applic (Var "zero?", [Var "n"]),
                                            Seq [Applic (Var "f", [Var "x"]); Applic (Var "g", [Var "y"])],
                                            Applic
                                              (LambdaSimple (["value"; "f"],
                                                If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),
                                                Const Void)),
                                              [Applic (Var "h?", [Var "x"]);
                                              LambdaSimple ([], Applic (Var "p", [Var "q"]))])))
            (Tag_Parser.tag_parse_expression (Pair (Symbol "cond",
                                                Pair
                                                  (Pair (Pair (Symbol "zero?", Pair (Symbol "n", Nil)),
                                                    Pair (Pair (Symbol "f", Pair (Symbol "x", Nil)),
                                                    Pair (Pair (Symbol "g", Pair (Symbol "y", Nil)), Nil))),
                                                  Pair
                                                  (Pair (Pair (Symbol "h?", Pair (Symbol "x", Nil)),
                                                    Pair (Symbol "=>",
                                                      Pair (Pair (Symbol "p", Pair (Symbol "q", Nil)), Nil))),
                                                  Nil)))));;


let test_tag_cond_expression_parser_7 text_ctxt = assert_equal (If (Applic (Var "zero?", [Var "n"]),
                                        Seq [Applic (Var "f", [Var "x"]); Applic (Var "g", [Var "y"])], Const Void))
            (Tag_Parser.tag_parse_expression (Pair (Symbol "cond",
                                                Pair
                                                  (Pair (Pair (Symbol "zero?", Pair (Symbol "n", Nil)),
                                                    Pair (Pair (Symbol "f", Pair (Symbol "x", Nil)),
                                                    Pair (Pair (Symbol "g", Pair (Symbol "y", Nil)), Nil))),
                                                  Nil))));;


let test_tag_cond_expression_parser_8 text_ctxt = assert_equal (If (Var "x", Var "x",
                                            Applic
                                              (LambdaSimple (["value"; "f"; "rest"],
                                                If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),
                                                Applic (Var "rest", []))),
                                              [LambdaSimple (["x"], Var "x");
                                              LambdaSimple ([], LambdaSimple (["x"], Applic (Var "x", [])));
                                              LambdaSimple ([], Var "value")])))    

                    (Tag_Parser.tag_parse_expression (Pair (Symbol "cond", Pair (Pair (Symbol "x", Pair (Pair (Symbol "begin",
                    Pair (Symbol "x", Nil)), Nil)), Pair (Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "x", Nil),
                      Pair (Symbol "x", Nil))), Pair (Symbol "=>", Pair (Pair (Symbol "cond", Pair (Pair (Symbol "else", Pair 
                      (Pair (Symbol "lambda", Pair (Pair (Symbol "x", Nil), Pair (Pair (Symbol "x", Nil), Nil))), Nil)), Nil)), Nil))),
                      Pair (Pair (Symbol "else", Pair (Symbol "value", Nil)), Nil))))));;

(* Name the test cases and group them together *)
let tag_cond_expression_expension_tester_suite =
"tag_cond_expression_parser_tester_suite">:::
 ["test_tag_cond_expression_parser_0">:: test_tag_cond_expression_parser_0;
  "test_tag_cond_expression_parser_1">:: test_tag_cond_expression_parser_1;
  "test_tag_cond_expression_parser_2">:: test_tag_cond_expression_parser_2;
  "test_tag_cond_expression_parser_3">:: test_tag_cond_expression_parser_3;
  "test_tag_cond_expression_parser_4">:: test_tag_cond_expression_parser_4;
  "test_tag_cond_expression_parser_5">:: test_tag_cond_expression_parser_5;
  "test_tag_cond_expression_parser_6">:: test_tag_cond_expression_parser_6;
  "test_tag_cond_expression_parser_7">:: test_tag_cond_expression_parser_7;
  "test_tag_cond_expression_parser_8">:: test_tag_cond_expression_parser_8;
  ]
;;

let () =
  run_test_tt_main tag_cond_expression_expension_tester_suite
;;
