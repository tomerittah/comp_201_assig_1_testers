#use "topfind";;
#require "oUnit";;

#use "tag-parser.ml";;

open Tag_Parser;;
open OUnit2;;

(* (lambda (x y) x y) *)
let test_tag_lambda_expression_parser_1 test_ctxt = assert_equal (LambdaSimple(["x"; "y"],
                                                                  Seq([Var "x"; Var "y"])))
  (Tag_Parser.tag_parse_expression (Pair (Symbol "lambda",
                                     Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),
                                      Pair (Symbol "x", Pair (Symbol "y", Nil))))));;

(* (lambda (x) (+ (+ x 2) 2)) *)
let test_tag_lambda_expression_parser_2 test_ctxt = assert_equal (LambdaSimple (["x"],
                                                                   Applic (Var "+",
                                                                    [Applic (Var "+", [Var "x"; Const (Sexpr (Number (Int 2)))]);
                                                                     Const (Sexpr (Number (Int 2)))]))
                                                                  )
  (Tag_Parser.tag_parse_expression (Pair (Symbol "lambda",
                                     Pair (Pair (Symbol "x", Nil),
                                      Pair
                                       (Pair (Symbol "+",
                                         Pair (Pair (Symbol "+", Pair (Symbol "x", Pair (Number (Int 2), Nil))),
                                          Pair (Number (Int 2), Nil))),
                                       Nil)))
                                    ));;

(* (lambda (x y . z) x y) *)
let test_tag_lambda_opt_expression_parser_1 test_ctxt = assert_equal (LambdaOpt(["x"; "y"],
                                                                        "z",
                                                                          Seq([Var "x"; Var "y"])))
  (Tag_Parser.tag_parse_expression (Pair (Symbol "lambda",
                                     Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "z")),
                                      Pair (Symbol "x", Pair (Symbol "y", Nil))))));;

let test_tag_lambda_variadic_expression_parser_1 test_ctxt = assert_equal (LambdaOpt ([], "x", Var "x"))
  (Tag_Parser.tag_parse_expression (Pair (Symbol "lambda", Pair (Symbol "x", Pair (Symbol "x", Nil)))));;

let test_tag_lambda_variadic_expression_parser_2 test_ctxt = assert_equal (Applic (LambdaOpt ([], "x", Var "x"),
                                                                           [Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 2)));
                                                                            Const (Sexpr (Number (Int 3))); Const (Sexpr (Number (Int 4)))]))
  (Tag_Parser.tag_parse_expression (Pair (Pair (Symbol "lambda", Pair (Symbol "x", Pair (Symbol "x", Nil))),
                                     Pair (Number (Int 1),
                                      Pair (Number (Int 2), Pair (Number (Int 3), Pair (Number (Int 4), Nil)))))));;


(* Name the test cases and group them together *)
let tag_lambda_expression_parser_tester_suite =
"tag_lambda_expression_parser_tester_suite">:::
 ["test_tag_lambda_expression_parser_1">:: test_tag_lambda_expression_parser_1;
  "test_tag_lambda_expression_parser_2">:: test_tag_lambda_expression_parser_2;
  "test_tag_lambda_opt_expression_parser_1">:: test_tag_lambda_opt_expression_parser_1;

  "test_tag_lambda_variadic_expression_parser_1">:: test_tag_lambda_variadic_expression_parser_1;
  "test_tag_lambda_variadic_expression_parser_2">:: test_tag_lambda_variadic_expression_parser_2;
  ]
;;

let () =
  run_test_tt_main tag_lambda_expression_parser_tester_suite
;;
