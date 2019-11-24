#use "topfind";;
#require "oUnit";;

#use "reader.ml";;

open Reader;;
open OUnit2;;

let test_program_1 test_ctxt = assert_equal ([Pair (Symbol "define", Pair (Symbol "x", Pair (Number (Int 1), Nil)))])
                                            (Reader.read_sexprs "(define x 1)");;
let test_program_2 test_ctxt = assert_equal ([TaggedSexpr ("x", Number (Int 1));
                                             TaggedSexpr ("y", Bool true);
                                             Pair (Number (Int 1),
                                             Pair (Number (Int 2), Pair (Pair (TagRef "x", TagRef "y"), Nil)))])
                                            (Reader.read_sexprs "#{x}=1 #{y}=#t (1 2 (#{x} . #{y}))");;
let test_program_3 test_ctxt = assert_equal (TaggedSexpr ("sym",
                                             Pair (Number (Int 1),
                                              Pair (Number (Float 2.3),
                                               Pair (Number (Float 0.01),
                                                Pair (Bool true,
                                                 Pair (Pair (Symbol "unquote", Pair (Bool false, Nil)),
                                                  Pair
                                                   (Pair (Symbol "unquote-splicing",
                                                     Pair (Pair (Symbol "a", Number (Int 1295)), Nil)),
                                                   Pair (TagRef "sym", Nil))))))))
                                            )
                                            (Reader.read_sexpr "#{sym}=(1 2.3 10e-3 #t ,#F ,@(a . #36rZZ) #{sym});this is ;my list");;
let test_program_4 test_ctxt = assert_equal ([Pair (Symbol "nested",
                                            Pair
                                             (Pair (Symbol "nested",
                                               Pair
                                                (Pair (Symbol "nested",
                                                  Pair (Pair (Symbol "nested", Symbol "nested"),
                                                   Pair (Pair (Symbol "nested", Nil),
                                                    Pair
                                                     (Pair (Pair (Pair (Pair (Symbol "nested", Nil), Nil), Nil), Nil),
                                                     Nil)))),
                                                Nil)),
                                             Nil));
                                           Symbol "nested"])
 (Reader.read_sexprs "(nested (nested (nested (nested . nested) (nested) ((((nested))))))) #; #; nested nested nested")
let test_program_5 test_ctxt = assert_equal ([Pair (Pair (Number (Int 2), Nil), Pair (Number (Int 3), Nil))])
                                           (Reader.read_sexprs "(#; 1 (2 #; 3) #; (1 2) 3)")

let test_program_6 test_ctxt = assert_equal ([Pair (Pair (Number (Int 2), Nil), Pair (Number (Int 3), Pair (Number (Int 3), Nil)))])
                                           (Reader.read_sexprs "(#; 1 (2 #; 3) #; (1 2) 3 #; #; (1 #; 3) 2 3)")

let e2e_read_sexprs_tester_suite =
"e2e_read_sexprs_tester_suite">:::
 ["test_program_1">:: test_program_1;
  "test_program_2">:: test_program_2;
  "test_program_3">:: test_program_3;
  "test_program_4">:: test_program_4;
  "test_program_5">:: test_program_5;
  "test_program_6">:: test_program_6;
  ]
;;

let () =
  run_test_tt_main e2e_read_sexprs_tester_suite
;;
