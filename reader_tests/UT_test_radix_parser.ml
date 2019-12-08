#use "topfind";;
#require "oUnit";;

#use "reader.ml";;

open Reader;;
open OUnit2;;

(* TESTER: test_radix_parser.ml *)
let test_positive_base16 test_ctxt = assert_equal (Number (Float 17.5390625)) (Reader.read_sexpr "#16R11.8a");;
let test_positive_base16_spaced1 test_ctxt = assert_equal (Number (Float 17.5390625)) (Reader.read_sexpr "   #16R11.8a ");;
let test_positive_signed_base14_spaced2 test_ctxt = assert_equal (Number (Float (15.6224489795918373))) (Reader.read_sexpr "   #14R+11.8a ");;
let test_positive_signed_base7 test_ctxt = assert_equal (Number (Float (633.999583506872114))) (Reader.read_sexpr "   #7r+1563.6666 ");;



(* Name the test cases and group them together *)
let radix_parser_tester_suite =
  "radix_parser_tester_suite">:::
    ["test_positive_base16">:: test_positive_base16;
     "test_positive_base16_spaced1">:: test_positive_base16_spaced1;
     "test_positive_signed_base14_spaced2">:: test_positive_signed_base14_spaced2;
     "test_positive_signed_base7">:: test_positive_signed_base7;
    ];;

let () =
  run_test_tt_main radix_parser_tester_suite
;;
(* END TESTER: test_radix_parser.ml *)
