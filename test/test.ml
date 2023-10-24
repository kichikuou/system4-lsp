open Base
open Sys4c
open System4_lsp

let analyze source =
  let doc = Document.create (Ain.create 4 0) source in
  if List.is_empty doc.errors then Stdio.print_endline "ok"
  else
    List.iter doc.errors ~f:(fun (range, message) ->
        Stdio.printf "(%d, %d) - (%d, %d) %s\n" range.start.line
          range.start.character range.end_.line range.end_.character message)

let%expect_test "empty doc" =
  analyze {||};
  [%expect {| ok |}]

let%expect_test "empty function" =
  analyze {|
    void f() {}
  |};
  [%expect {| ok |}]

let%expect_test "syntax error" =
  analyze {|
    int c = ;
  |};
  [%expect {| (1, 12) - (1, 13) Syntax error. |}]

let%expect_test "undefined variable" =
  analyze {|
    int c = foo;
  |};
  [%expect {| (1, 12) - (1, 15) Undefined variable: foo |}]

let%expect_test "arity error" =
  analyze {|
    int c = system.Exit();
  |};
  [%expect
    {| (1, 12) - (1, 25) Arity error. 'Exit' expects 1 arguments, but 0 provided. |}]

let%expect_test "not lvalue error" =
  analyze {|
    ref int c = 3;
  |};
  [%expect {| (1, 12) - (1, 17) Lvalue expected. |}]

let%expect_test "undefined type error" =
  analyze {|
    undef_t c;
  |};
  [%expect {| (1, 12) - (1, 13) Undefined type: undef_t |}]

let%expect_test "type error" =
  analyze {|
    void f() { int x = "s"; }
  |};
  [%expect
    {|
      (1, 19) - (1, 26) Type error.
       Expected type: int
       Actual type: string |}]
