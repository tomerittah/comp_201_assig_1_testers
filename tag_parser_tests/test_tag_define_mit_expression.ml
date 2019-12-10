#use "topfind";;
#require "oUnit";;

#use "tag-parser.ml";;

open Tag_Parser;;
open OUnit2;;

(* (define (x . (y)) 23) -> (define x (lambda (y) 23)) *)
let test_tag_define_mit_expression_parser_1 test_ctxt = assert_equal (Def (Var "x", LambdaSimple (["y"], Const (Sexpr (Number (Int 23))))))
  (Tag_Parser.tag_parse_expression (Pair (Symbol "define",
                                     Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),
                                      Pair (Number (Int 23), Nil)))));;

(* (define (x . (y z)) (+ y z)) -> (define x (lambda (y z) (+ y z))) *)
let test_tag_define_mit_expression_parser_2 test_ctxt = assert_equal (Def (Var "x",
                                                          LambdaSimple (["y"; "z"], Applic (Var "+", [Var "y"; Var "z"]))))
  (Tag_Parser.tag_parse_expression (Pair (Symbol "define",
                                     Pair (Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil))),
                                      Pair (Pair (Symbol "+", Pair (Symbol "y", Pair (Symbol "z", Nil))), Nil)))));;

(* (define (x . (a b c)) #t (+ b c) (+ (- 23 a) b) (let ((d 3)) (+ d a))) ->
    (define x (lambda (a b c) #t (+ b c) (+ (- 23 a) b) (let ((d 3)) (+ d a)))) *)
let test_tag_define_mit_expression_parser_3 test_ctxt = assert_equal (Def (Var "x",
                                                                       LambdaSimple (["a"; "b"; "c"],
                                                                        Seq
                                                                         [Const (Sexpr (Bool true)); Applic (Var "+", [Var "b"; Var "c"]);
                                                                          Applic (Var "+",
                                                                           [Applic (Var "-", [Const (Sexpr (Number (Int 23))); Var "a"]); Var "b"]);
                                                                          Applic (LambdaSimple (["d"], Applic (Var "+", [Var "d"; Var "a"])),
                                                                           [Const (Sexpr (Number (Int 3)))])])))
  (Tag_Parser.tag_parse_expression (Pair (Symbol "define",
                                     Pair
                                      (Pair (Symbol "x",
                                        Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil)))),
                                      Pair (Bool true,
                                       Pair (Pair (Symbol "+", Pair (Symbol "b", Pair (Symbol "c", Nil))),
                                        Pair
                                         (Pair (Symbol "+",
                                           Pair
                                            (Pair (Symbol "-", Pair (Number (Int 23), Pair (Symbol "a", Nil))),
                                            Pair (Symbol "b", Nil))),
                                         Pair
                                          (Pair (Symbol "let",
                                            Pair (Pair (Pair (Symbol "d", Pair (Number (Int 3), Nil)), Nil),
                                             Pair (Pair (Symbol "+", Pair (Symbol "d", Pair (Symbol "a", Nil))),
                                              Nil))),
                                          Nil))))))
                                    ));;

(* Name the test cases and group them together *)
let tag_define_mit_expression_parser_tester_suite =
"tag_define_mit_expression_parser_tester_suite">:::
 ["test_tag_define_mit_expression_parser_1">:: test_tag_define_mit_expression_parser_1;
  "test_tag_define_mit_expression_parser_2">:: test_tag_define_mit_expression_parser_2;
  "test_tag_define_mit_expression_parser_3">:: test_tag_define_mit_expression_parser_3
  ]
;;

let () =
  run_test_tt_main tag_define_mit_expression_parser_tester_suite
;;
