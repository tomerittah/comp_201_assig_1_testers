#use "topfind";;
#require "oUnit";;

#use "tag-parser.ml";;

open Tag_Parser;;
open OUnit2;;

(* (let ((x 5)) (+ 5 x)) -> ((lambda (x) (+ 5 x)) 5) *)
let test_tag_let_expression_parser_1 test_ctxt = assert_equal (Applic
                                                               (LambdaSimple (["x"],
                                                                 Applic (Var "+", [Const (Sexpr (Number (Int 5))); Var "x"])),
                                                               [Const (Sexpr (Number (Int 5)))]))
  (Tag_Parser.tag_parse_expression (Pair (Symbol "let",
                                     Pair (Pair (Pair (Symbol "x", Pair (Number (Int 5), Nil)), Nil),
                                      Pair (Pair (Symbol "+", Pair (Number (Int 5), Pair (Symbol "x", Nil))),
                                       Nil)))));;

(* (let ((x 3) (y #t)) (if y x 2)) -> ((lambda (x y) (if y x 2)) 3 #t) *)
let test_tag_let_expression_parser_2 test_ctxt = assert_equal (Applic
                                                               (LambdaSimple (["x"; "y"],
                                                                 If (Var "y", Var "x", Const (Sexpr (Number (Int 2))))),
                                                               [Const (Sexpr (Number (Int 3))); Const (Sexpr (Bool true))]))
 (Tag_Parser.tag_parse_expression (Pair (Symbol "let",
                                     Pair
                                      (Pair (Pair (Symbol "x", Pair (Number (Int 3), Nil)),
                                        Pair (Pair (Symbol "y", Pair (Bool true, Nil)), Nil)),
                                      Pair
                                       (Pair (Symbol "if",
                                         Pair (Symbol "y", Pair (Symbol "x", Pair (Number (Int 2), Nil)))),
                                       Nil)))
                                    ));;

(* (let () (+ 5 5)) -> ((lambda () (+ 5 5))) *)
let test_tag_let_expression_parser_3 test_ctxt = assert_equal (Applic
                                                               (LambdaSimple ([],
                                                                 Applic (Var "+",
                                                                    [Const (Sexpr (Number (Int 5))); Const (Sexpr (Number (Int 5)))])),
                                                               [])
                                                              )
 (Tag_Parser.tag_parse_expression (Pair (Symbol "let",
                                     Pair (Nil,
                                      Pair (Pair (Symbol "+", Pair (Number (Int 5), Pair (Number (Int 5), Nil))),
                                       Nil)))
                                    ));;

(* (let ((x 5)) (let ((y #f)) (let ((z #t)) (if (or z y) x 'not)))) ->
   ((((lambda (x) (lambda (y) (lambda (z) (if (or z y) x 'not)))) 5) #f) #t) *)
let test_tag_let_expression_parser_4 test_ctxt = assert_equal (Applic
                                                               (LambdaSimple (["x"],
                                                                  Applic
                                                                    (LambdaSimple (["y"],
                                                                       Applic
                                                                         (LambdaSimple (["z"],
                                                                            If (Or [Var "z"; Var "y"], Var "x",
                                                                              Const (Sexpr (Symbol "not")))),
                                                                         [Const (Sexpr (Bool true))])),
                                                                    [Const (Sexpr (Bool false))])),
                                                               [Const (Sexpr (Number (Int 5)))]))

 (Tag_Parser.tag_parse_expression (Pair (Symbol "let",
                                     Pair (Pair (Pair (Symbol "x", Pair (Number (Int 5), Nil)), Nil),
                                      Pair
                                       (Pair (Symbol "let",
                                         Pair (Pair (Pair (Symbol "y", Pair (Bool false, Nil)), Nil),
                                          Pair
                                           (Pair (Symbol "let",
                                             Pair (Pair (Pair (Symbol "z", Pair (Bool true, Nil)), Nil),
                                              Pair
                                               (Pair (Symbol "if",
                                                 Pair
                                                  (Pair (Symbol "or", Pair (Symbol "z", Pair (Symbol "y", Nil))),
                                                  Pair (Symbol "x",
                                                   Pair (Pair (Symbol "quote", Pair (Symbol "not", Nil)), Nil)))),
                                               Nil))),
                                           Nil))),
                                       Nil)))));;

(* (let ((x 5) (y #f) (z #t)) (if (or z y) x 'not)) -> ((lambda (x y z) (if (or z y) x 'not)) 5 #f #t) *)
let test_tag_let_expression_parser_5 test_ctxt = assert_equal (Applic
                                             (LambdaSimple (["x"; "y"; "z"],
                                               If (Or [Var "z"; Var "y"], Var "x", Const (Sexpr (Symbol "not")))),
                                             [Const (Sexpr (Number (Int 5))); Const (Sexpr (Bool false));
                                              Const (Sexpr (Bool true))]))
              (Tag_Parser.tag_parse_expression (Pair (Symbol "let",
                                                 Pair
                                                  (Pair (Pair (Symbol "x", Pair (Number (Int 5), Nil)),
                                                    Pair (Pair (Symbol "y", Pair (Bool false, Nil)),
                                                     Pair (Pair (Symbol "z", Pair (Bool true, Nil)), Nil))),
                                                  Pair
                                                   (Pair (Symbol "if",
                                                     Pair (Pair (Symbol "or", Pair (Symbol "z", Pair (Symbol "y", Nil))),
                                                      Pair (Symbol "x",
                                                       Pair (Pair (Symbol "quote", Pair (Symbol "not", Nil)), Nil)))),
                                                   Nil)))));;

(* (let ((x (+ 5 5))) x) -> ((lambda (x) x) (+ 5 5)) *)
let test_tag_let_expression_parser_6 test_ctxt = assert_equal (Applic (LambdaSimple (["x"], Var "x"),
                                                               [Applic (Var "+",
                                                                 [Const (Sexpr (Number (Int 5))); Const (Sexpr (Number (Int 5)))])]))
                                     (Tag_Parser.tag_parse_expression (Pair (Symbol "let",
                                                                        Pair
                                                                         (Pair
                                                                           (Pair (Symbol "x",
                                                                             Pair
                                                                              (Pair (Symbol "+", Pair (Number (Int 5), Pair (Number (Int 5), Nil))),
                                                                              Nil)),
                                                                           Nil),
                                                                         Pair (Symbol "x", Nil)))));;

let test_tag_let_expression_parser_7 test_ctxt = assert_equal (Applic
                                                                 (LambdaSimple (["x"; "y"],
                                                                    Applic
                                                                      (LambdaSimple (["z"],
                                                                        Applic (Var "*", [Var "x"; Var "y"; Var "z"])),
                                                                      [Applic (Var "+",
                                                                        [Const (Sexpr (Number (Int 4))); Const (Sexpr (Number (Int 4)))])])),
                                                                 [Applic (Var "+",
                                                                   [Const (Sexpr (Number (Int 5))); Const (Sexpr (Number (Int 5)))]);
                                                                  Applic (Var "*",
                                                                   [Const (Sexpr (Number (Int 3))); Const (Sexpr (Number (Int 3)))])])
                                                                )

                          (Tag_Parser.tag_parse_expression (Pair (Symbol "let",
                                                             Pair
                                                              (Pair
                                                                (Pair (Symbol "x",
                                                                  Pair
                                                                   (Pair (Symbol "+", Pair (Number (Int 5), Pair (Number (Int 5), Nil))),
                                                                   Nil)),
                                                                Pair
                                                                 (Pair (Symbol "y",
                                                                   Pair
                                                                    (Pair (Symbol "*", Pair (Number (Int 3), Pair (Number (Int 3), Nil))),
                                                                    Nil)),
                                                                 Nil)),
                                                              Pair
                                                               (Pair (Symbol "let",
                                                                 Pair
                                                                  (Pair
                                                                    (Pair (Symbol "z",
                                                                      Pair
                                                                       (Pair (Symbol "+",
                                                                         Pair (Number (Int 4), Pair (Number (Int 4), Nil))),
                                                                       Nil)),
                                                                    Nil),
                                                                  Pair
                                                                   (Pair (Symbol "*",
                                                                     Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil)))),
                                                                   Nil))),
                                                               Nil)))));;

 (* (let* () #t #f #t) -> (let () #t #f #t) -> ((lambda () #t #f #t)) *)
 let test_tag_let_star_expression_parser_1 test_ctxt = assert_equal (Applic
                                                                     (LambdaSimple ([],
                                                                       Seq
                                                                        [Const (Sexpr (Bool true)); Const (Sexpr (Bool false));
                                                                         Const (Sexpr (Bool true))]),
                                                                     []))
                  (Tag_Parser.tag_parse_expression (Pair (Symbol "let*",
                                                      Pair (Nil, Pair (Bool true,
                                                        Pair (Bool false,
                                                          Pair (Bool true, Nil)))))));;

(* (let* ((x 5)) (+ x 5)) -> (let ((x 5)) (+ x 5)) -> ((lambda (x) (+ x 5)) 5) *)
let test_tag_let_star_expression_parser_2 test_ctxt = assert_equal (Applic
                                                         (LambdaSimple (["x"],
                                                           Applic (Var "+", [Var "x"; Const (Sexpr (Number (Int 5)))])),
                                                         [Const (Sexpr (Number (Int 5)))]))
                   (Tag_Parser.tag_parse_expression (Pair (Symbol "let*",
                                                       Pair (Pair (Pair (Symbol "x", Pair (Number (Int 5), Nil)), Nil),
                                                        Pair (Pair (Symbol "+", Pair (Symbol "x", Pair (Number (Int 5), Nil))),
                                                         Nil)))
                                                      ));;

(* (let* ((x 5) (y 6)) (+ x y)) -> (let ((x 5)) (let ((y 6)) (+ x y))) -> ((lambda (x) ((lambda (y) (+ x y)) 6) 5)) *)
let test_tag_let_star_expression_parser_3 test_ctxt = assert_equal (Applic
                                                                     (LambdaSimple (["x"],
                                                                       Applic
                                                                          (LambdaSimple (["y"], Applic (Var "+", [Var "x"; Var "y"])),
                                                                          [Const (Sexpr (Number (Int 6)))])),
                                                                     [Const (Sexpr (Number (Int 5)))])
                                                                    )
                   (Tag_Parser.tag_parse_expression (Pair (Symbol "let*",
                                                       Pair
                                                        (Pair (Pair (Symbol "x", Pair (Number (Int 5), Nil)),
                                                          Pair (Pair (Symbol "y", Pair (Number (Int 6), Nil)), Nil)),
                                                        Pair (Pair (Symbol "+", Pair (Symbol "x", Pair (Symbol "y", Nil))), Nil)))));;

(* (let* ((x 5) (y #f) (z #t)) (if (or z y) x 'not)))) -> ((((lambda (x) (lambda (y) (lambda (z) (if (or z y) x 'not)))) 5) #f) #t) *)
let test_tag_let_star_expression_parser_4 test_ctxt = assert_equal (Applic
                                                             (LambdaSimple (["x"],
                                                                Applic
                                                                  (LambdaSimple (["y"],
                                                                    Applic
                                                                       (LambdaSimple (["z"],
                                                                         If (Or [Var "z"; Var "y"], Var "x",
                                                                            Const (Sexpr (Symbol "not")))),
                                                                       [Const (Sexpr (Bool true))])),
                                                                  [Const (Sexpr (Bool false))])),
                                                             [Const (Sexpr (Number (Int 5)))]))

(Tag_Parser.tag_parse_expression (Pair (Symbol "let*",
                                   Pair
                                    (Pair (Pair (Symbol "x", Pair (Number (Int 5), Nil)),
                                      Pair (Pair (Symbol "y", Pair (Bool false, Nil)),
                                       Pair (Pair (Symbol "z", Pair (Bool true, Nil)), Nil))),
                                    Pair
                                     (Pair (Symbol "if",
                                       Pair (Pair (Symbol "or", Pair (Symbol "z", Pair (Symbol "y", Nil))),
                                        Pair (Symbol "x",
                                         Pair (Pair (Symbol "quote", Pair (Symbol "not", Nil)), Nil)))),
                                     Nil)))));;

(* (let* ((x 5) (y x) (z (+ 2 x))) (+ z x y)) -> (let ((x 5)) (let ((y x)) (let ((z (+ 2 x))) (+ z x y)))) ->
   ((lambda (x) ((lambda (y) ((lambda (z) (+ z x y)) (+ 2 x))) x)) 5) *)
let test_tag_let_star_expression_parser_5 test_ctxt = assert_equal (Applic
                                                               (LambdaSimple (["x"],
                                                                  Applic
                                                                    (LambdaSimple (["y"],
                                                                       Applic
                                                                         (LambdaSimple (["z"],
                                                                           Applic (Var "+", [Var "z"; Var "x"; Var "y"])),
                                                                         [Applic (Var "+", [Const (Sexpr (Number (Int 2))); Var "x"])])),
                                                                    [Var "x"])),
                                                               [Const (Sexpr (Number (Int 5)))])
                                                              )

                      (Tag_Parser.tag_parse_expression (Pair (Symbol "let*",
                                                         Pair
                                                          (Pair (Pair (Symbol "x", Pair (Number (Int 5), Nil)),
                                                            Pair (Pair (Symbol "y", Pair (Symbol "x", Nil)),
                                                             Pair
                                                              (Pair (Symbol "z",
                                                                Pair
                                                                 (Pair (Symbol "+", Pair (Number (Int 2), Pair (Symbol "x", Nil))),
                                                                 Nil)),
                                                              Nil))),
                                                          Pair
                                                           (Pair (Symbol "+",
                                                             Pair (Symbol "z", Pair (Symbol "x", Pair (Symbol "y", Nil)))),
                                                           Nil)))
                                                        ));;

(* (letrec () 2)  *)
let test_tag_let_rec_expression_parser_1 test_ctxt = assert_equal (Applic (LambdaSimple ([], Const (Sexpr (Number (Int 2)))), []))
                      (Tag_Parser.tag_parse_expression (Pair (Symbol "letrec", Pair (Nil, Pair (Number (Int 2), Nil)))));;

(* (letrec ((fact
      (lambda (n)
        (if (zero? n) 1
          (\* n (fact (- n 1)))))))
    (fact 5)) ->
    (let ((fact 'whatever)) (set! fact (lambda (n)
        (if (zero? n)
        1
        (\* n (fact (- n 1)))))) (fact 5)) ->
        ((lambda (fact) (set! fact (lambda (n)
        (if (zero? n)
        1
        (\* n (fact (- n 1)))))) (fact 5)) 'whatever)
      *)
let test_tag_let_rec_expression_parser_2 test_ctxt = assert_equal (Applic
                                                                   (LambdaSimple (["fact"],
                                                                     Seq
                                                                      [Set (Var "fact",
                                                                        LambdaSimple (["n"],
                                                                         If (Applic (Var "zero?", [Var "n"]), Const (Sexpr (Number (Int 1))),
                                                                          Applic (Var "*",
                                                                           [Var "n";
                                                                            Applic (Var "fact",
                                                                             [Applic (Var "-", [Var "n"; Const (Sexpr (Number (Int 1)))])])]))));
                                                                       Applic (Var "fact", [Const (Sexpr (Number (Int 5)))])]),
                                                                   [Const (Sexpr (Symbol "whatever"))])
                                                                  )
                      (Tag_Parser.tag_parse_expression (Pair (Symbol "letrec",
                                                         Pair
                                                          (Pair
                                                            (Pair (Symbol "fact",
                                                              Pair
                                                               (Pair (Symbol "lambda",
                                                                 Pair (Pair (Symbol "n", Nil),
                                                                  Pair
                                                                   (Pair (Symbol "if",
                                                                     Pair (Pair (Symbol "zero?", Pair (Symbol "n", Nil)),
                                                                      Pair (Number (Int 1),
                                                                       Pair
                                                                        (Pair (Symbol "*",
                                                                          Pair (Symbol "n",
                                                                           Pair
                                                                            (Pair (Symbol "fact",
                                                                              Pair
                                                                               (Pair (Symbol "-",
                                                                                 Pair (Symbol "n", Pair (Number (Int 1), Nil))),
                                                                               Nil)),
                                                                            Nil))),
                                                                        Nil)))),
                                                                   Nil))),
                                                               Nil)),
                                                            Nil),
                                                          Pair (Pair (Symbol "fact", Pair (Number (Int 5), Nil)), Nil)))
                                                        ));;

  (* (letrec ((square (lambda (x) ( * x x))) (y 5)) (square y)) ->
     (let ((square 'whatever) (y 'whatever)) (set! square (lambda (x) ( * x x))) (set! y 5) (square y)) ->
     ((lambda (square y) (set! square (lambda (x) ( * x x))) (set! y 5) (square y)) 'whatever 'whatever)
  *)
  let test_tag_let_rec_expression_parser_3 test_ctxt = assert_equal (Applic
                                                                       (LambdaSimple (["square"; "y"],
                                                                         Seq
                                                                          [Set (Var "square",
                                                                            LambdaSimple (["x"], Applic (Var "*", [Var "x"; Var "x"])));
                                                                           Set (Var "y", Const (Sexpr (Number (Int 5))));
                                                                           Applic (Var "square", [Var "y"])]),
                                                                       [Const (Sexpr (Symbol "whatever")); Const (Sexpr (Symbol "whatever"))])
                                                                      )
      (Tag_Parser.tag_parse_expression (Pair (Symbol "letrec",
                                         Pair
                                          (Pair
                                            (Pair (Symbol "square",
                                              Pair
                                               (Pair (Symbol "lambda",
                                                 Pair (Pair (Symbol "x", Nil),
                                                  Pair (Pair (Symbol "*", Pair (Symbol "x", Pair (Symbol "x", Nil))),
                                                   Nil))),
                                               Nil)),
                                            Pair (Pair (Symbol "y", Pair (Number (Int 5), Nil)), Nil)),
                                          Pair (Pair (Symbol "square", Pair (Symbol "y", Nil)), Nil)))
                                        ));;

  (* (letrec ((mul (lambda (x y) ( * x y))) (y 5) (z 3)) (mul y z)) ->
     (let ((mul 'whatever) (y 'whatever) (z 'whatever)) (set! mul (lambda (x y) ( * x y))) (set! y 5) (set! z 3) (mul y z)) ->
     ((lambda (mul y z) (set! mul (lambda (x y) ( * x y))) (set! y 5) (set! z 3) (mul y z)) 'whatever 'whatever 'whatever)
  *)
  let test_tag_let_rec_expression_parser_4 test_ctxt = assert_equal (Applic
                                                                     (LambdaSimple (["mul"; "y"; "z"],
                                                                       Seq
                                                                        [Set (Var "mul",
                                                                          LambdaSimple (["x"; "y"], Applic (Var "*", [Var "x"; Var "y"])));
                                                                         Set (Var "y", Const (Sexpr (Number (Int 5))));
                                                                         Set (Var "z", Const (Sexpr (Number (Int 3))));
                                                                         Applic (Var "mul", [Var "y"; Var "z"])]),
                                                                     [Const (Sexpr (Symbol "whatever")); Const (Sexpr (Symbol "whatever"));
                                                                      Const (Sexpr (Symbol "whatever"))]))
      (Tag_Parser.tag_parse_expression (Pair (Symbol "letrec",
                                         Pair
                                          (Pair
                                            (Pair (Symbol "mul",
                                              Pair
                                               (Pair (Symbol "lambda",
                                                 Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),
                                                  Pair (Pair (Symbol "*", Pair (Symbol "x", Pair (Symbol "y", Nil))),
                                                   Nil))),
                                               Nil)),
                                            Pair (Pair (Symbol "y", Pair (Number (Int 5), Nil)),
                                             Pair (Pair (Symbol "z", Pair (Number (Int 3), Nil)), Nil))),
                                          Pair (Pair (Symbol "mul", Pair (Symbol "y", Pair (Symbol "z", Nil))), Nil)))
                                        ));;

(* Name the test cases and group them together *)
let tag_let_expression_parser_tester_suite =
"tag_let_expression_parser_tester_suite">:::
 ["test_tag_let_expression_parser_1">:: test_tag_let_expression_parser_1;
  "test_tag_let_expression_parser_2">:: test_tag_let_expression_parser_2;
  "test_tag_let_expression_parser_3">:: test_tag_let_expression_parser_3;
  "test_tag_let_expression_parser_4">:: test_tag_let_expression_parser_4;
  "test_tag_let_expression_parser_5">:: test_tag_let_expression_parser_5;
  "test_tag_let_expression_parser_6">:: test_tag_let_expression_parser_6;
  "test_tag_let_expression_parser_7">:: test_tag_let_expression_parser_7;

  "test_tag_let_star_expression_parser_1">:: test_tag_let_star_expression_parser_1;
  "test_tag_let_star_expression_parser_2">:: test_tag_let_star_expression_parser_2;
  "test_tag_let_star_expression_parser_3">:: test_tag_let_star_expression_parser_3;
  "test_tag_let_star_expression_parser_4">:: test_tag_let_star_expression_parser_4;
  "test_tag_let_star_expression_parser_5">:: test_tag_let_star_expression_parser_5;

  "test_tag_let_rec_expression_parser_1">:: test_tag_let_rec_expression_parser_1;
  "test_tag_let_rec_expression_parser_2">:: test_tag_let_rec_expression_parser_2;
  "test_tag_let_rec_expression_parser_3">:: test_tag_let_rec_expression_parser_3;
  "test_tag_let_rec_expression_parser_4">:: test_tag_let_rec_expression_parser_4;
  ]
;;

let () =
  run_test_tt_main tag_let_expression_parser_tester_suite
;;
