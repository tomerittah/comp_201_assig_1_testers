#use "topfind";;
#require "oUnit";;

#use "reader.ml";;

open Reader;;
open OUnit2;;

(* TESTER: test_tag_parser.ml *)
let test_tag_1 test_ctxt = assert_equal (TagRef ("T10")) (Reader.read_sexpr "#{T10}");;
let test_tag_2 test_ctxt = assert_equal (TagRef ("T10")) (Reader.read_sexpr "# {T10}");;
let test_tagged_exp_1 test_ctxt = assert_equal (TaggedSexpr("x", Number (Int(1)))) (Reader.read_sexpr "#{x}=1");;
let test_tagged_exp_2 test_ctxt = assert_equal (TaggedSexpr ("x", Pair (Symbol "a", TagRef "x")))
                                  (Reader.read_sexpr "#{x}=(a . #{x})");;
let test_tagged_exp_3 test_ctxt = assert_equal ([TaggedSexpr ("foo", (Pair (Number (Int (1)), Pair (Number (Int (2)), Pair ((Number (Int (3)), Nil))))));
                                                 Pair(Number( Int(1)), Pair(TaggedSexpr("foo", Number( Int(2))), Pair(TagRef ("foo"), Nil)))])
                                  (Reader.read_sexprs "#{foo}=(1 2 3) (1 #{foo}=2 #{foo})");;
let test_tagged_exp_4 test_ctxt = assert_equal (
  TaggedSexpr ("foo",
   Pair (Number (Int 2),
    Pair (Number (Int 3),
     Pair
      (Pair (Number (Int 2),
        Pair (Number (Int 3),
         Pair (Pair (Number (Int 1), Pair (TagRef "foo", Nil)), Nil))), Nil))))) (Reader.read_sexpr "#{foo}=(2 3 (2 3 (1 #{foo})))");;
let test_tagged_exp_5 test_ctxt = assert_equal (TaggedSexpr ("x", Pair (Symbol "a", TagRef "x")))
                                 (Reader.read_sexpr ";comment \n #;  #;  #{x}=(a . #{x}) (1 2) #{x}=(a . #{x})   ");;

let test_tag_raises_1 test_ctxt = assert_raises X_this_should_not_happen (fun _ ->
                                  (Reader.read_sexpr "#{foo}=(#{foo}=1 2 3)"));;
let test_tag_raises_2 test_ctxt = assert_raises X_this_should_not_happen (fun _ ->
                                  (Reader.read_sexpr "#{foo}=(2 3 (2 3 (1 #{foo}=3)))"));;
let test_tag_raises_3 test_ctxt = assert_raises X_this_should_not_happen (fun _ ->
                                  (Reader.read_sexpr "#{foo}=#{foo}=2"));;




(* Name the test cases and group them together *)
let tag_parser_tester_suite =
"tag_parser_tester_suite">:::
 ["test_tag_1">:: test_tag_1;
  "test_tag_2">:: test_tag_2;
  "test_tagged_exp_1">:: test_tagged_exp_1;
  "test_tagged_exp_2">:: test_tagged_exp_2;
  "test_tagged_exp_3">:: test_tagged_exp_3;
  "test_tagged_exp_4">:: test_tagged_exp_4;
  "test_tagged_exp_5">:: test_tagged_exp_5;

  "test_tag_raises_1">:: test_tag_raises_1;
  "test_tag_raises_2">:: test_tag_raises_2;
  "test_tag_raises_3">:: test_tag_raises_3;
  ]
;;

let () =
  run_test_tt_main tag_parser_tester_suite
;;
(* END TESTER: test_tag_parser.ml *)
