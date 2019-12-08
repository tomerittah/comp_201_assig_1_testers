#use "topfind";;
#require "oUnit";;

#use "tag-parser.ml";;

open Tag_Parser;;
open OUnit2;;

let test_tag_constant_parser_1 test_ctxt = assert_equal (Const(Sexpr (Bool (true))))
  (Tag_Parser.tag_parse_expression (Bool (true)));;

let test_tag_constant_parser_2 test_ctxt = assert_equal (Const(Sexpr (Number (Float(1.23)))))
  (Tag_Parser.tag_parse_expression (Number (Float(1.23))));;

let test_tag_constant_parser_3 test_ctxt = assert_equal (Const(Sexpr (Char ('a'))))
(Tag_Parser.tag_parse_expression (Char ('a')));;

let test_tag_constant_parser_4 test_ctxt = assert_equal (Const(Sexpr (String ("hello"))))
  (Tag_Parser.tag_parse_expression (String ("hello")));;

let test_tag_constant_parser_5 test_ctxt = assert_equal (Const(Sexpr (TaggedSexpr ("x", Nil))))
  (Tag_Parser.tag_parse_expression (TaggedSexpr ("x", Pair (Symbol "quote", Pair (Nil, Nil)))));;

let test_tag_constant_parser_6 test_ctxt = assert_equal (Const(Sexpr (TaggedSexpr ("x", Pair(Symbol "quote", Pair (Nil, Nil))))))
  (Tag_Parser.tag_parse_expression (Pair (Symbol "quote", Pair(TaggedSexpr
                                  ("x", Pair (Symbol "quote", Pair (Nil, Nil))), Nil))));;

let test_tag_constant_parser_7 test_ctxt = assert_equal (Const(Sexpr (Bool (false))))
  (Tag_Parser.tag_parse_expression (Bool (false)));;

let test_tag_constant_parser_8 test_ctxt = assert_equal (Const(Sexpr (TagRef ("x"))))
  (Tag_Parser.tag_parse_expression (TagRef ("x")));;

  let test_tag_constant_parser_9 test_ctxt = assert_equal (Const(Sexpr (TaggedSexpr ("x", Bool(true)))))
    (Tag_Parser.tag_parse_expression (TaggedSexpr ("x", Pair (Symbol "quote", Pair (Bool true, Nil)))));;

(* Name the test cases and group them together *)
let tag_constant_parser_tester_suite =
"tag_constant_parser_tester_suite">:::
 ["test_tag_constant_parser_1">:: test_tag_constant_parser_1;
  "test_tag_constant_parser_2">:: test_tag_constant_parser_2;
  "test_tag_constant_parser_3">:: test_tag_constant_parser_3;
  "test_tag_constant_parser_4">:: test_tag_constant_parser_4;
  "test_tag_constant_parser_5">:: test_tag_constant_parser_5;
  "test_tag_constant_parser_6">:: test_tag_constant_parser_6;
  "test_tag_constant_parser_7">:: test_tag_constant_parser_7;
  "test_tag_constant_parser_8">:: test_tag_constant_parser_8;
  "test_tag_constant_parser_9">:: test_tag_constant_parser_9;
  ]
;;

let () =
  run_test_tt_main tag_constant_parser_tester_suite
;;
