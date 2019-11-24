#use "topfind";;
#require "oUnit";;

#use "reader.ml";;

open Reader;;
open OUnit2;;

(* TESTER: test_string_parser.ml *)
let test_simple_string test_ctxt = assert_equal (String "abc") (Reader.read_sexpr "\"abc\"");;
let test_simple_string1 test_ctxt = assert_equal (String "  dbc  ") (Reader.read_sexpr " \"  dbc  \"    ");;

let test_special_string test_ctxt = assert_equal (String "\\'\'") (Reader.read_sexpr "\"\\\\''\"");;
let test_complex_string test_ctxt = assert_equal (String "A4ge4...\\\"\\\"\\\\\\\\\\\\\\\\ \\t lo \\t \\r \\\\ \\n  tr3")
  (Reader.read_sexpr "    \"A4ge4...\\\\\\\"\\\\\\\"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ \\\\t lo \\\\t \\\\r \\\\\\\\ \\\\n  tr3\"     ");;
let test_complex_string1 test_ctxt = assert_equal (String "hello \t \r \\ \n  world!")
  (Reader.read_sexpr "\"hello \\t \\r \\\\ \\n  world!\"");;
let test_complex_string2 test_ctxt = assert_equal (String "A4ge4...\"\"\\\\\\t\t\t\t\t\t\t\n\\\\\\\'\'\'\"\" \t lo \t \r \\ \n  tr3")
  (Reader.read_sexpr "\"A4ge4...\\\"\\\"\\\\\\\\\\\\t\\t\\t\\t\\t\\t\\t\\n\\\\\\\\\\\\'''\\\"\\\" \\t lo \\t \\r \\\\ \\n  tr3\"");;
(* let test_complex_string1 test_ctxt = assert_equal (String "hello \t \r \\ \n  world!")
  (fst (String.nt_string "\"hello \\t \\r \\\\ \\n  world!\"")));;
let test_complex_string1 test_ctxt = assert_equal (String "hello \t \r \\ \n  world!")
  (fst (String.nt_string "\"hello \\t \\r \\\\ \\n  world!\"")));; *)

let string_parser_tester_suite =
  "string_parser_tester_suite">:::
   ["test_simple_string">:: test_simple_string;
    "test_simple_string1">:: test_simple_string1;
    "test_special_string">:: test_special_string;
    "test_complex_string">:: test_complex_string;
    "test_complex_string1">:: test_complex_string1;
    "test_complex_string2">:: test_complex_string2;
   ]

let () =
      run_test_tt_main string_parser_tester_suite
    ;;
(* END TESTER: test_string_parser.ml *)
