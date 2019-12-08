#use "topfind";;
#require "oUnit";;

#use "tag-parser.ml";;

open Tag_Parser;;
open OUnit2;;


let test_tag_Applic_expression_parser_1 test_ctxt = assert_equal (LambdaSimple (["x"; "y"],
                                                                   Seq
                                                                    [Applic (Var "+", [Var "x"; Var "y"]);
                                                                     Applic (Var "*", [Var "x"; Var "y"])]))

  (Tag_Parser. tag_parse_expression (Pair (Symbol "lambda",
                                      Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),
                                      Pair (Pair (Symbol "+", Pair (Symbol "x", Pair (Symbol "y", Nil))),
                                       Pair (Pair (Symbol "*", Pair (Symbol "x", Pair (Symbol "y", Nil))), Nil))))));;


let test_tag_Applic_expression_parser_2 test_ctxt = assert_equal (Applic (Var "+", [Var "x"; Var "y"]))
                        (Tag_Parser.tag_parse_expression (Pair (Symbol "+", Pair (Symbol "x", Pair (Symbol "y", Nil)))));;

let test_tag_Applic_expression_parser_3 test_ctxt = assert_equal (Applic (Var "*", [Var "x"; Var "y"; Var "z"; Var "k"]))
                        (Tag_Parser.tag_parse_expression (Pair (Symbol "*", Pair (Symbol "x", Pair (Symbol "y",Pair (Symbol "z",
                                                          Pair (Symbol "k", Nil)))))));;

let test_tag_Applic_expression_parser_4 test_ctxt = assert_equal (Applic
                                                                   (LambdaSimple (["a"; "b"; "c"],
                                                                     Applic (Var "+", [Var "a"; Var "b"; Var "c"])),
                                                                   [Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 2)));
                                                                    Const (Sexpr (Number (Int 3)))]))

                        (Tag_Parser.tag_parse_expression (Pair
                                                           (Pair (Symbol "lambda",
                                                             Pair (Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil))),
                                                              Pair
                                                               (Pair (Symbol "+",
                                                                 Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil)))),
                                                               Nil))),
                                                           Pair (Number (Int 1), Pair (Number (Int 2), Pair (Number (Int 3), Nil))))));;

let test_tag_Applic_expression_parser_5 test_ctxt = assert_equal (Applic
                                                             (LambdaOpt (["a"; "b"], "c",
                                                                If (Or [Var "a"; Var "b"], Var "c",
                                                                  Applic (Var "+", [Var "a"; Var "b"]))),
                                                             [Const (Sexpr (Number (Int 5))); Const (Sexpr (Number (Int 6)));
                                                              Const (Sexpr (String "Hi, I'm LambdaOpt Applic "))]))
                        (Tag_Parser.tag_parse_expression (Pair
                                                         (Pair (Symbol "lambda",
                                                           Pair (Pair (Symbol "a", Pair (Symbol "b", Symbol "c")),
                                                            Pair
                                                             (Pair (Symbol "if",
                                                               Pair (Pair (Symbol "or", Pair (Symbol "a", Pair (Symbol "b", Nil))),
                                                                Pair (Symbol "c",
                                                                 Pair (Pair (Symbol "+", Pair (Symbol "a", Pair (Symbol "b", Nil))),
                                                                  Nil)))),
                                                             Nil))),
                                                         Pair (Number (Int 5),
                                                          Pair (Number (Int 6), Pair (String "Hi, I'm LambdaOpt Applic ", Nil))))));;

let test_tag_Applic_expression_parser_6 test_ctxt = assert_equal (Applic
                                                     (LambdaSimple (["x"],
                                                        LambdaSimple (["y"],
                                                           LambdaOpt (["z"], "c",
                                                             Seq
                                                              [Set (Var "x", Const (Sexpr (Number (Int 5))));
                                                               Set (Var "y", Const (Sexpr (Number (Float (-0.4321)))));
                                                               If (Or [Var "z"; Const (Sexpr (Bool true))],
                                                                Const (Sexpr (Symbol "c")), Const Void)]))),
                                                     [Const (Sexpr (Number (Float 3.2))); Const (Sexpr (Number (Float 4.8)));
                                                      Const (Sexpr (Bool true)); Const (Sexpr (String "this is curry"))]))
                                    (Tag_Parser.tag_parse_expression (Pair
                                                                 (Pair (Symbol "lambda",
                                                                 Pair (Pair (Symbol "x", Nil),
                                                                  Pair
                                                                   (Pair (Symbol "lambda",
                                                                     Pair (Pair (Symbol "y", Nil),
                                                                      Pair
                                                                       (Pair (Symbol "lambda",
                                                                         Pair (Pair (Symbol "z", Symbol "c"),
                                                                          Pair
                                                                           (Pair (Symbol "set!",
                                                                             Pair (Symbol "x", Pair (Number (Int 5), Nil))),
                                                                           Pair
                                                                            (Pair (Symbol "set!",
                                                                              Pair (Symbol "y", Pair (Number (Float (-0.4321)), Nil))),
                                                                            Pair
                                                                             (Pair (Symbol "if",
                                                                               Pair
                                                                                (Pair (Symbol "or",
                                                                                  Pair (Symbol "z", Pair (Bool true, Nil))),
                                                                                  Pair (Pair (Symbol "quote", Pair (Symbol "c", Nil)), Nil))),
                                                                             Nil))))),
                                                                       Nil))),
                                                                   Nil))),
                                                               Pair (Number (Float 3.2),
                                                                Pair (Number (Float 4.8),
                                                                 Pair (Bool true, Pair (String "this is curry", Nil)))))));;


(* (lambda (x) x) (lambda (y) y) *)
 let test_tag_Applic_expressions_parser_1 test_ctxt = assert_equal ([LambdaSimple (["x"], Var "x"); LambdaSimple (["y"], Var "y")])

   (Tag_Parser.tag_parse_expressions ([Pair (Symbol "lambda",
                                      Pair (Pair (Symbol "x", Nil), Pair (Symbol "x", Nil)));
                                     Pair (Symbol "lambda",
                                      Pair (Pair (Symbol "y", Nil), Pair (Symbol "y", Nil)))]));;

(* (lambda (x) (+ (+ x 3) (+ x 2))) *)
let test_tag_Applic_expressions_parser_2 test_ctxt = assert_equal ([LambdaSimple (["x"],
                                                                     Applic (Var "+",
                                                                      [Applic (Var "+", [Var "x"; Const (Sexpr (Number (Int 3)))]);
                                                                       Applic (Var "+", [Var "x"; Const (Sexpr (Number (Int 2)))])]))])

  (Tag_Parser.tag_parse_expressions ([Pair (Symbol "lambda",
                                       Pair (Pair (Symbol "x", Nil),
                                        Pair
                                         (Pair (Symbol "+",
                                             Pair (Pair (Symbol "+", Pair (Symbol "x", Pair (Number (Int 3), Nil))),
                                            Pair (Pair (Symbol "+", Pair (Symbol "x", Pair (Number (Int 2), Nil))),
                                             Nil))),
                                           Nil)))]));;

(* (if x x x) (or 1 2 3) (define (y . (z)) (+ y z)) *)
 let test_tag_Applic_expressions_parser_3 test_ctxt = assert_equal ([If (Var "x", Var "x", Var "x");
                                                                     Or
                                                                     [Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 2)));
                                                                     Const (Sexpr (Number (Int 3)))];
                                                                    Def (Var "y",
                                                                    LambdaSimple (["z"],
                                                                    Applic (Var "+", [Var "y"; Var "z"])))
                                                                    ])

  (Tag_Parser.tag_parse_expressions ([Pair (Symbol "if",
                                      Pair (Symbol "x", Pair (Symbol "x", Pair (Symbol "x", Nil))));
                                       Pair (Symbol "or",
                                       Pair (Number (Int 1), Pair (Number (Int 2), Pair (Number (Int 3), Nil))));
                                       Pair (Symbol "define",
                                          Pair (Pair (Symbol "y", Pair (Symbol "z", Nil)),
                                            Pair (Pair (Symbol "+", Pair (Symbol "y", Pair (Symbol "z", Nil))), Nil)))]
                                            ));;

(* Name the test cases and group them together *)
let tag_applic_expression_parser_tester_suite =
"tag_applic_expression_parser_tester_suite">:::
 ["test_tag_Applic_expression_parser_1">:: test_tag_Applic_expression_parser_1;
  "test_tag_Applic_expression_parser_2">:: test_tag_Applic_expression_parser_2;
  "test_tag_Applic_expression_parser_3">:: test_tag_Applic_expression_parser_3;
  "test_tag_Applic_expression_parser_4">:: test_tag_Applic_expression_parser_4;
  "test_tag_Applic_expression_parser_5">:: test_tag_Applic_expression_parser_5;
  "test_tag_Applic_expression_parser_6">:: test_tag_Applic_expression_parser_6;

  "test_tag_Applic_expressions_parser_1">:: test_tag_Applic_expressions_parser_1;
  "test_tag_Applic_expressions_parser_2">:: test_tag_Applic_expressions_parser_2;
  "test_tag_Applic_expressions_parser_3">:: test_tag_Applic_expressions_parser_3;
  ]
;;

let () =
  run_test_tt_main tag_applic_expression_parser_tester_suite
;;
