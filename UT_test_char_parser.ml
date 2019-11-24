#use "topfind";;
#require "oUnit";;

#use "reader.ml";;

open Reader;;
open OUnit2;;

(* TESTER: test_char_parser.ml *)
let test_newline test_ctxt = assert_equal (Char '\n') (Reader.read_sexpr " #\\newline");;
let test_newline_with_spaces test_ctxt = assert_equal (Char '\n') (Reader.read_sexpr "   #\\newline ");;
let test_newline_with_spaces2 test_ctxt = assert_equal (Char '\n') (Reader.read_sexpr "   #\\nEwline ");;
let test_return_with_spaces test_ctxt = assert_equal (Char '\r') (Reader.read_sexpr "   #\\return    ");;
let test_charS_with_spaces test_ctxt = assert_equal (Char 'S') (Reader.read_sexpr "   #\\S  ");;
let test_chara test_ctxt = assert_equal (Char 'a') (Reader.read_sexpr "   #\\a ");;
let test_page_with_spaces test_ctxt = assert_equal (Char '\012') (Reader.read_sexpr "   #\\page    ");;
let test_page_with_spaces2 test_ctxt = assert_equal (Char '\012') (Reader.read_sexpr "   #\\pAGE    ");;
let test_char_with_extras test_ctxt = assert_equal [Char 'A'; Symbol "b"] (Reader.read_sexprs "#\\Ab");;

(* Name the test cases and group them together *)
let char_parser_tester_suite =
"char_parser_tester_suite">:::
 ["test_newline">:: test_newline;
  "test_newline_with_spaces">:: test_newline_with_spaces;
  "test_newline_with_spaces2">:: test_newline_with_spaces2;
  "test_return_with_spaces">:: test_return_with_spaces;
  "test_charS_with_spaces">:: test_charS_with_spaces;
  "test_chara">:: test_chara;
  "test_page_with_spaces">:: test_page_with_spaces;
  "test_page_with_spaces2">:: test_page_with_spaces2;
  "test_char_with_extras">:: test_char_with_extras;
  ]
;;

let () =
  run_test_tt_main char_parser_tester_suite
;;
(* END TESTER: test_char_parser.ml *)
