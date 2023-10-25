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

let%expect_test "RefAssign operator" =
  analyze
    {|
      struct S { int f; ref int rf; };
      ref int ref_val() { return NULL; }
      void f() {
        int a = 1, b = 2;
        ref int ra = a, rb = b;
        S s;
        ra <- rb;         // ok
        ra <- a;          // ok
        a <- ra;          // error: lhs is not a reference
        NULL <- ra;       // error: lhs can't be the NULL keyword
        ra <- NULL;       // ok
        ra <- ref_val();  // ok
        ra <- 3;          // error: rhs is not a lvalue
        ref_val() <- ra;  // error: lhs is not a variable
        s.rf <- ra;       // ok
        s.f <- ra;        // error: lhs is not a reference
      }
    |};
  [%expect
    {|
    (9, 8) - (9, 16) Type error.
     Expected type: ref int
     Actual type: int
    (10, 8) - (10, 19) Type error.
     Expected type: ref int
     Actual type: null
    (13, 8) - (13, 16) Lvalue expected.
    (14, 8) - (14, 24) Type error.
     Expected type: ref int
     Actual type: ref int
    (16, 8) - (16, 18) Type error.
     Expected type: ref int
     Actual type: int |}]

let%expect_test "RefEqual operator" =
  analyze
    {|
      struct S { int f; ref int rf; };
      ref int ref_val() { return NULL; }
      void f() {
        int a = 1, b = 2;
        ref int ra = a, rb = b;
        S s;
        ra === rb;         // ok
        ra === a;          // ok
        a === ra;          // error: lhs is not a reference
        NULL === ra;       // error: lhs can't be the NULL keyword
        ra === NULL;       // ok
        ra === ref_val();  // ok
        ra === 3;          // error: rhs is not a lvalue
        ref_val() === ra;  // ok
        s.rf === ra;       // ok
        s.f === ra;        // error: lhs is not a reference
      }
    |};
  [%expect
    {|
       (9, 8) - (9, 16) Type error.
        Expected type: ref int
        Actual type: int
       (10, 8) - (10, 19) Type error.
        Expected type: null
        Actual type: int
       (13, 8) - (13, 16) Lvalue expected.
       (16, 8) - (16, 18) Type error.
        Expected type: ref int
        Actual type: int |}]
