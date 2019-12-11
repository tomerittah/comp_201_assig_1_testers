#use "topfind";;
#require "oUnit";;

#use "tag-parser.ml";;

open Tag_Parser;;
open OUnit2;;

let test_mix_1 test_ctxt = assert_equal (Const (Sexpr (Nil)))
  (Tag_Parser.tag_parse_expression (Pair (Symbol "quote", Pair (Nil, Nil))));;

let test_mix_2 test_ctxt = assert_equal (Const (Sexpr (Pair (String "strin", Pair (Char 'g', Nil)))))
  (Tag_Parser.tag_parse_expression (Pair (Symbol "quote", Pair (Pair (String "strin", Pair (Char 'g', Nil)), Nil))));;

let test_mix_3 test_ctxt = assert_equal (Var "1x")
  (Tag_Parser.tag_parse_expression (Symbol "1x"));;

let test_mix_4 test_ctxt = assert_equal (LambdaSimple ([], Seq ([Const (Sexpr (Pair
                                 (Number (Int (1)), Pair (Number (Int (2)), Nil))));Const (Sexpr (Number (Int (2))))])))
  (Tag_Parser.tag_parse_expression (Pair (Symbol "lambda", Pair (Nil, Pair (Pair
   (Symbol "quote", Pair (Pair (Number (Int (1)), Pair (Number (Int (2)), Nil)), Nil)), Pair (Number (Int (2)), Nil))))));;

let test_mix_5 test_ctxt = assert_equal (Def (Var "x", Const (Sexpr (Number (Int (1))))))
  (Tag_Parser.tag_parse_expression (Pair (Symbol "define", Pair (Symbol "x", Pair (Number (Int (1)), Nil)))));;


let test_mix_6 test_ctxt = assert_equal (Def (Var "123x", Const (Sexpr (Number (Int (123))))))
  (Tag_Parser.tag_parse_expression (Pair (Symbol "define", Pair (Symbol "123x", Pair (Number (Int (123)), Nil)))));;

let test_mix_7 test_ctxt = assert_equal (LambdaSimple (["x"], If (Var "x", Var "x", Const (Sexpr (Bool false)))))
  (Tag_Parser.tag_parse_expression (Pair (Symbol "lambda", Pair (Pair (Symbol "x", Nil), Pair
   (Pair (Symbol "if", Pair (Symbol "x", Pair (Symbol "x", Pair (Bool false, Nil)))), Nil)))));;

let test_mix_8 test_ctxt = assert_equal (LambdaSimple (["x"], If (Var "x", Var "x", Const (Sexpr (Bool false)))))
  (Tag_Parser.tag_parse_expression (Pair (Symbol "lambda", Pair (Pair (Symbol "x", Nil), Pair
   (Pair (Symbol "if", Pair (Symbol "x", Pair (Symbol "x", Pair (Bool false, Nil)))), Nil)))));;

let test_mix_9 test_ctxt = assert_equal (Applic (LambdaSimple (["x";"y"], Applic (Var "eq?", 
                                    [Var "x";Var "y"])), [If (Var "x", Var "x", Const (Void));Applic (Var "void", [])]))
  (Tag_Parser.tag_parse_expression (Pair (Symbol "let", Pair (Pair (Pair 
            (Symbol "x", Pair (Pair (Symbol "if", Pair (Symbol "x", Pair (Symbol "x", Nil))), Nil)), Pair (Pair (Symbol "y",
                    Pair (Pair (Symbol "void", Nil), Nil)), Nil)), Pair (Pair
                     (Symbol "eq?", Pair (Symbol "x", Pair (Symbol "y", Nil))), Nil)))));;

let test_mix_10 test_ctxt = assert_equal (Applic (LambdaSimple ([], Applic (LambdaSimple (["loop"], Seq ([Set (Var "loop",
                    LambdaSimple (["x"], If (Applic (Var "null?", [Var "x"]), Const (Sexpr (Symbol "done")), Applic (Var "loop",
                    [Applic (Var "cdr", [Var "x"])]))));Applic (Var "loop", [Applic (Var "cons", [Var "a";
                    Applic (Var "cons", [Var "b";Applic (Var "cons", [Var "c";Const (Sexpr (Nil))])])])])])), 
                    [Const (Sexpr (Symbol "whatever"))])), []))
            (Tag_Parser.tag_parse_expression (Pair(Symbol "let", Pair(Nil, Pair(Pair(Symbol "letrec", Pair(Pair(Pair
            (Symbol "loop", Pair(Pair(Symbol "lambda", Pair(Pair(Symbol "x", Nil), Pair(Pair(Symbol "if", Pair
            (Pair(Symbol "null?", Pair(Symbol "x", Nil)), Pair(Pair(Symbol "quote", Pair(Symbol "done", Nil)), 
            Pair(Pair(Symbol "loop", Pair(Pair(Symbol "cdr", Pair(Symbol "x", Nil)), Nil)), Nil)))), Nil))), Nil)), Nil), 
            Pair(Pair(Symbol "loop", Pair(Pair(Symbol "quasiquote", Pair(Pair(Pair(Symbol "unquote", Pair(Symbol "a", Nil)), Pair
            (Pair(Symbol "unquote", Pair(Symbol "b", Nil)), Pair(Pair(Symbol "unquote", Pair(Symbol "c", Nil)), Nil))), Nil)),
            Nil)), Nil))), Nil)))));;

let test_mix_11 test_ctxt = assert_equal (Applic (Or ([If
                    (LambdaSimple ([], Var "<test>"), LambdaSimple ([], Var "<then>"),
                    Const (Sexpr (Bool false)));LambdaSimple ([], Var "<else>")]), []))
                    (Tag_Parser.tag_parse_expression (Pair (Pair (Symbol "or", Pair (Pair (Symbol "and", Pair
                    (Pair (Symbol "lambda", Pair (Nil, Pair (Symbol "<test>", Nil))), Pair (Pair (Symbol "lambda", 
                    Pair (Nil, Pair (Symbol "<then>", Nil))), Nil))), Pair (Pair (Symbol "lambda", Pair
                    (Nil, Pair (Symbol "<else>", Nil))), Nil))), Nil)));;

let test_mix_12 test_ctxt = assert_equal (Applic (LambdaSimple (["x"], Applic (LambdaSimple (["y"], 
                                    Applic (Var "cons", [Const (Sexpr (Symbol "y"));Applic (Var "cons",
                                    [Applic (Var "cdr", [Var "x"]);Const (Sexpr (Nil))])])),
                                    [Applic (Var "car", [Var "x"])])), [Applic (Var "cons",
                                    [Const (Sexpr (Symbol "a"));Applic (Var "cons", [Const (Sexpr (Symbol "="));Applic (Var "append",
                                    [Var "a";Const (Sexpr (Nil))])])])]))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "let*", Pair (Pair (Pair (Symbol "x", Pair (Pair (Symbol "quasiquote",
                     Pair (Pair (Symbol "a", Pair (Symbol "=", Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)), Nil))),
                      Nil)), Nil)), Pair (Pair (Symbol "y", Pair (Pair (Symbol "car", Pair (Symbol "x", Nil)), Nil)), Nil)),
                       Pair (Pair (Symbol "quasiquote", 
                    Pair (Pair (Symbol "y", Pair (Pair (Symbol "unquote", 
                    Pair (Pair (Symbol "cdr", Pair (Symbol "x", Nil)), Nil)), Nil)), Nil)), Nil)))));;

let test_mix_13 test_ctxt = assert_equal (LambdaSimple (["x"], Applic (LambdaSimple ([], Set (Var "x", Var "y")), [])))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "lambda", Pair (Pair (Symbol "x", Nil), Pair (Pair
                                                (Symbol "let", Pair (Nil, Pair (Pair (Symbol "set!", Pair
                                                (Symbol "x", Pair (Symbol "y", Nil))), Nil))), Nil)))));;

let test_mix_14 test_ctxt = assert_equal (Def (Var "x", LambdaOpt (["y"], "z", Applic (Var "eq?",
                                                 [Const (Sexpr (String "y"));Var "y"]))))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "define", Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "z")),
                                             Pair (Pair (Symbol "eq?", Pair (String "y", Pair (Symbol "y", Nil))), Nil)))));;


let test_mix_15 test_ctxt = assert_equal (Def (Var "x", LambdaOpt ([], "z", Applic (Var "eq?",
                                         [Const (Sexpr (Char 'y'));Applic (Var "car", [Var "z"])]))))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "define", Pair (Pair (Symbol "x", Symbol "z"),
                     Pair (Pair (Symbol "eq?", Pair (Char 'y', Pair (Pair (Symbol "car", 
                     Pair (Symbol "z", Nil)), Nil))), Nil)))));;

let test_mix_16 test_ctxt = assert_equal (Def (Var "x", LambdaOpt ([], "y", Applic
                                             (Var "eq?", [Const (Sexpr (Nil));Var "y"]))))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "define", Pair (Pair (Symbol "x", Symbol "y"),
                     Pair (Pair (Symbol "eq?", Pair (Pair (Symbol "quote", Pair (Nil, Nil)), Pair (Symbol "y", Nil))), Nil)))));;


let test_mix_17 test_ctxt = assert_equal (Applic (LambdaSimple (["loop"], Seq ([Set (Var "loop", LambdaSimple (["x"],
                                        If (Var "x", Set (Var "y", 
                            Const (Sexpr (Bool true))), Applic (Var "loop", [Var "y"]))));Applic (Var "loop", [Var "x"])])), 
                                [Const (Sexpr (Symbol "whatever"))]))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "letrec", Pair (Pair (Pair (Symbol "loop",
                     Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "x", Nil), Pair (Pair (Symbol "if",
                      Pair (Symbol "x", Pair (Pair (Symbol "set!", Pair (Symbol "y", Pair (Bool true, Nil))), Pair (Pair
                     (Symbol "loop", Pair (Symbol "y", Nil)), Nil)))), Nil))), Nil)), Nil), 
                     Pair (Pair (Symbol "loop", Pair (Symbol "x", Nil)), Nil)))));;


let test_mix_18 test_ctxt = assert_equal (If (Const (Sexpr (Bool true)),
                        Seq ([Const (Sexpr (Number (Int (1))));Const
                        (Sexpr (Number (Int (2))));Const (Sexpr (Number (Int (3))))]), Const (Void)))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "cond",
                     Pair (Pair (Bool true, Pair (Number (Int (1)),
                      Pair (Number (Int (2)), Pair (Number (Int (3)), Nil)))), Nil))));;



let test_mix_19 test_ctxt = assert_equal (Applic (LambdaSimple (["x"], Applic (LambdaSimple 
                (["y"], Seq [Var "x"; Applic (Var "y", [])]), [LambdaSimple ([], Var "x")])),
                 [Const (Sexpr (Number (Int (1))))]))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "let*", Pair (Pair (Pair (Symbol "x", Pair (Number (Int (1)), Nil)),
                     Pair (Pair (Symbol "y", Pair (Pair
                     (Symbol "lambda", Pair (Nil, Pair (Symbol "x", Nil))), Nil)), Nil)),
                      Pair (Symbol "x", Pair (Pair (Symbol "y", Nil), Nil))))));;


let test_mix_20 test_ctxt = assert_equal (If (If (Var "a", Var "b", Const (Sexpr (Char 'c'))), 
                                        Const (Sexpr (Symbol "x")), Const (Sexpr (Pair (Symbol "+", Pair (Symbol "y", Pair 
                                        (Symbol "z", Nil)))))))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "if", Pair (Pair
                     (Symbol "if", Pair (Symbol "a", Pair (Symbol "b", Pair (Char 'c', Nil)))),
                      Pair (Pair (Symbol "quote", Pair (Symbol "x", Nil)),
                     Pair (Pair (Symbol "quote", Pair (Pair (Symbol "+", Pair (Symbol "y", Pair (Symbol "z", Nil))),
                      Nil)), Nil))))));;


let test_mix_21 test_ctxt = assert_equal (Or ([Applic (Var "zero?", [Var "x"]);Applic (Var "zero?", [Var "y"]);Applic 
                                (Var "zero?", [Var "z"]);Applic (Var "zero?", [Var "w"]);Applic (Var "zero?", [Var "v"])]))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "or", Pair (Pair (Symbol "zero?", Pair (Symbol "x", Nil)), Pair 
                    (Pair (Symbol "zero?", Pair (Symbol "y", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "z", Nil)), 
                    Pair (Pair (Symbol "zero?", Pair (Symbol "w", Nil)), Pair
                     (Pair (Symbol "zero?", Pair (Symbol "v", Nil)), Nil))))))));;


let test_mix_22 test_ctxt = assert_equal (If (Applic (Var "zero?", [Var "x"]), If (Applic (Var "zero?", [Var "y"]),
                                    If (Applic (Var "zero?", [Var "z"]), If (Applic (Var "zero?", [Var "w"]), 
                                    Applic (Var "zero?", [Var "v"]),
                                 Const (Sexpr (Bool false))), Const (Sexpr (Bool false))), Const (Sexpr (Bool false))),
                                  Const (Sexpr (Bool false))))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "and", Pair (Pair (Symbol "zero?", Pair (Symbol "x", Nil)),
                     Pair (Pair (Symbol "zero?", Pair (Symbol "y", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "z", Nil)),
                     Pair (Pair (Symbol "zero?", Pair (Symbol "w", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "v", Nil)), Nil))))))));;


let test_mix_23 test_ctxt = assert_equal (If (Seq ([Const (Sexpr (Number 
                        (Int (1))));Const (Sexpr (Number (Int (2))));Const (Sexpr (Number (Int (3))));Const (Sexpr (Bool false))]), 
                        Const (Sexpr (Number (Int (1)))), LambdaSimple ([], Seq ([Const (Void);Const (Sexpr (Number (Int (1))));Const
                        (Sexpr (Number (Int (2))));Const (Sexpr (Number (Int (3))));Const (Sexpr (Bool false))]))))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "if", Pair (Pair (Symbol "begin", Pair (Number (Int (1)),
                     Pair (Number (Int (2)), Pair (Number (Int (3)), Pair (Bool false, Nil))))), Pair (Number (Int (1)),
                      Pair (Pair (Symbol "lambda", Pair (Nil, Pair (Pair (Symbol "begin", Nil), 
                    Pair (Number (Int (1)), Pair (Number (Int (2)), Pair (Number (Int (3)), Pair (Bool false, Nil))))))), Nil))))));;


let test_mix_24 test_ctxt = assert_equal (Def (Var "foo", LambdaSimple (["x";"y"], Seq
                                         ([LambdaOpt ([], "y", Seq ([Var "y";Var "y"]));LambdaOpt (["z"],
                                     "w", Seq ([Var "z";Var "w"]));LambdaSimple (["z"], LambdaOpt ([], "w", Var "w"))]))))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "define", Pair (Pair (Symbol "foo", Pair (Symbol "x", Pair (Symbol "y", Nil))),
                                        Pair (Pair (Symbol "lambda", Pair (Symbol "y", Pair (Symbol "y", Pair (Symbol "y", Nil)))), 
                                        Pair (Pair (Symbol "lambda", 
                                        Pair (Pair (Symbol "z", Symbol "w"), Pair (Symbol "z", Pair (Symbol "w", Nil)))),
                                         Pair (Pair (Symbol "lambda", 
                                        Pair (Pair (Symbol "z", Nil), Pair (Pair (Symbol "lambda", Pair (Symbol "w",
                                         Pair (Symbol "w", Nil))), Nil))), Nil)))))));;

let test_mix_25 test_ctxt = assert_equal (Applic (Var "cons", [Var "a";Applic
                             (Var "cons", [Const (Sexpr (Symbol "b"));Applic 
                                (Var "append", [Var "c";Const (Sexpr (Nil))])])]))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "quasiquote", Pair (Pair (Pair (Symbol "unquote",
                     Pair (Symbol "a", Nil)), 
                            Pair (Symbol "b", Pair (Pair (Symbol "unquote-splicing", 
                            Pair (Symbol "c", Nil)), Nil))), Nil))));;

let test_mix_26 test_ctxt = assert_equal (Applic (Applic (Var "append", 
                            [Var "x";Applic (Var "cons", [Set (Var "y", 
                            Applic (Var "car", [Var "x"]));Const (Sexpr (Nil))])]), []))
                    (Tag_Parser.tag_parse_expression (Pair (Pair (Symbol "quasiquote", Pair
                     (Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "x", Nil)), Pair (Pair (Symbol "unquote", Pair (Pair
                     (Symbol "set!", Pair (Symbol "y", Pair (Pair (Symbol "car", Pair (Symbol "x", Nil)), Nil))),
                      Nil)), Nil)), Nil)), Nil)));;

let test_mix_27 test_ctxt = assert_equal (Applic (LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Symbol "let")))),
                                     [Applic (LambdaSimple (["x"], Applic
                                            (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Number (Int (2))))])), 
                                            [Const (Sexpr (Number (Int (1))))])]))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "let", Pair (Pair 
                    (Pair (Symbol "x", Pair (Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Int (1)), Nil)), Nil),
                     Pair (Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Int (2)), Nil)), Nil), Pair (Symbol "x", Nil))), 
                     Nil))), Nil)), Nil), Pair
                         (Pair (Symbol "set!", Pair (Symbol "x", Pair (Pair
                          (Symbol "quote", Pair (Symbol "let", Nil)), Nil))), Nil)))));;

let test_mix_28 test_ctxt = assert_equal (Applic (LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Symbol "let")))),
                                     [Applic (LambdaSimple (["x"], Applic
                                            (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Number (Int (2))))])), 
                                            [Const (Sexpr (Number (Int (1))))])]))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "let", Pair (Pair 
                    (Pair (Symbol "x", Pair (Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Int (1)), Nil)), Nil),
                     Pair (Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Int (2)), Nil)), Nil), Pair (Symbol "x", Nil))), 
                     Nil))), Nil)), Nil), Pair
                         (Pair (Symbol "set!", Pair (Symbol "x", Pair (Pair
                          (Symbol "quote", Pair (Symbol "let", Nil)), Nil))), Nil)))));;

let test_mix_29 test_ctxt = assert_equal (Or ([Const (Void);Const (Sexpr (Bool true))]))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "or", Pair (Pair (Symbol "begin", Nil),
                                     Pair (Pair (Symbol "and", Nil), Nil)))));;

let test_mix_30 test_ctxt = assert_equal (Applic (Var "+", [Const (Sexpr (Number (Int (1))));Const (Sexpr (Number (Int (2))))]))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "+", Pair (Pair (Symbol "and",
                                     Pair (Number (Int (1)), Nil)), Pair (Pair (Symbol "or", Pair (Number (Int (2)), Nil)), Nil)))));;

let test_mix_31 test_ctxt = assert_equal (Const (Sexpr (Pair (Symbol "cond", Pair (Pair (Symbol "let", Nil), 
                                            Pair (Pair (Symbol "or", Pair (Symbol "and", Nil)), Nil))))))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "quote", Pair (Pair 
                        (Symbol "cond", Pair (Pair (Symbol "let", Nil), Pair 
                        (Pair (Symbol "or", Pair (Symbol "and", Nil)), Nil))), Nil))));;

let test_mix_32 test_ctxt = assert_equal (Seq ([LambdaSimple ([], Seq 
                            ([Seq ([Const (Sexpr (Symbol "a"));Const (Sexpr (Symbol "b"))]);
                            Const (Sexpr (Symbol "c"))]));Const (Sexpr (Symbol "d"))]))
                    (Tag_Parser.tag_parse_expression (Pair (Symbol "begin", Pair (Pair (Symbol "lambda", Pair 
                    (Nil, Pair (Pair (Symbol "begin", Pair (Pair (Symbol "quote", Pair (Symbol "a", Nil)), Pair
                     (Pair (Symbol "quote", Pair (Symbol "b", Nil)), Nil))), 
                     Pair (Pair (Symbol "quote", Pair (Symbol "c", Nil)), Nil)))),
                                 Pair (Pair (Symbol "begin", Pair
                                  (Pair (Symbol "quote", Pair (Symbol "d", Nil)), Nil)), Nil)))));;

let test_mix_33 test_ctxt = assert_equal (Applic (LambdaSimple (["c"], Applic
                         (LambdaSimple (["a"], Seq ([Var "a";Const (Sexpr (Symbol "b"))])), 
                         [Var "c"])), [Const (Sexpr (Symbol "d"))]))
                    (Tag_Parser.tag_parse_expression (Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "c", Nil),
                         Pair (Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "a", Nil), Pair (Symbol "a", 
                         Pair (Pair (Symbol "quote", Pair (Symbol "b", Nil)), Nil)))), Pair (Symbol "c", Nil)), Nil))),
                             Pair (Pair (Symbol "begin", Pair (Pair (Symbol "quote", Pair (Symbol "d", Nil)), Nil)), Nil))));;

let test_mix_34 test_ctxt = assert_equal ([Const (Sexpr (Number (Int (1))));Const (Sexpr (Number (Int (2))));Var "x";Var "y";Var "z"])
                    (Tag_Parser.tag_parse_expressions ([Number (Int 1); Number (Int 2); Symbol "x"; Symbol "y"; Symbol "z"]));;


let test_mix_35 test_ctxt = assert_equal ([Def (Var "foo", LambdaSimple (["x"], Applic (Var "zero?", [Var "x"])));
                                                    Applic (LambdaSimple (["value";"f"], If (Var "value", Applic 
                                (Applic (Var "f", []), [Var "value"]), Const (Void))), 
                                [Const (Sexpr (Bool false));LambdaSimple ([], Var "foo")])])
                    (Tag_Parser.tag_parse_expressions ([Pair (Symbol "define",
                                            Pair (Pair (Symbol "foo", Pair (Symbol "x", Nil)),
                                            Pair (Pair (Symbol "zero?", Pair (Symbol "x", Nil)), Nil)));
                                            Pair (Symbol "cond",
                                            Pair
                                            (Pair (Bool false, Pair (Symbol "=>", Pair (Symbol "foo", Nil))),
                                            Nil))]));;


(* Name the test cases and group them together *)
let tag_mix_tester_suite =
"tag_mix_tester_suite">:::
 ["test_mix_1">:: test_mix_1;
  "test_mix_2">:: test_mix_2;
  "test_mix_3">:: test_mix_3;
  "test_mix_4">:: test_mix_4;
  "test_mix_5">:: test_mix_5;
  "test_mix_6">:: test_mix_6;
  "test_mix_7">:: test_mix_7;
  "test_mix_8">:: test_mix_8;
  "test_mix_9">:: test_mix_9;
  "test_mix_10">:: test_mix_10;
  "test_mix_11">:: test_mix_11;
  "test_mix_12">:: test_mix_12;
  "test_mix_13">:: test_mix_13;
  "test_mix_14">:: test_mix_14;
  "test_mix_15">:: test_mix_15;
  "test_mix_16">:: test_mix_16;
  "test_mix_17">:: test_mix_17;
  "test_mix_18">:: test_mix_18;
  "test_mix_19">:: test_mix_19;
  "test_mix_20">:: test_mix_20;
  "test_mix_21">:: test_mix_21;
  "test_mix_22">:: test_mix_22;
  "test_mix_23">:: test_mix_23;
  "test_mix_24">:: test_mix_24;
  "test_mix_25">:: test_mix_25;
  "test_mix_26">:: test_mix_26;
  "test_mix_27">:: test_mix_27;
  "test_mix_28">:: test_mix_28;
  "test_mix_29">:: test_mix_29;  
  "test_mix_30">:: test_mix_30;
  "test_mix_31">:: test_mix_31;
  "test_mix_32">:: test_mix_32;
  "test_mix_33">:: test_mix_33;
  "test_mix_34">:: test_mix_34;
  "test_mix_35">:: test_mix_35;
  ]
;;

let () =
  run_test_tt_main tag_mix_tester_suite
;;