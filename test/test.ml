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
      ref S ref_S() { return NULL; }
      void S::f(ref S other) {
        int a = 1, b = 2;
        ref int ra = a, rb = b;
        S s;
        ra <- rb;         // ok
        ra <- a;          // ok
        a <- ra;          // error: lhs is not a reference
        NULL <- ra;       // error: lhs can't be the NULL keyword
        ra <- NULL;       // ok
        ra <- ref_val();  // ok
        ra <- ref_S();    // error: referenced type mismatch
        ra <- 3;          // error: rhs is not a lvalue
        ref_val() <- ra;  // error: lhs is not a variable
        s.rf <- ra;       // ok
        s.f <- ra;        // error: lhs is not a reference
        other <- this;    // ok
        this <- other;    // error: lhs is not a reference
      }
    |};
  [%expect
    {|
      (10, 8) - (10, 16) Type error.
       Expected type: ref int
       Actual type: int
      (11, 8) - (11, 19) Type error.
       Expected type: ref int
       Actual type: null
      (14, 8) - (14, 22) Type error.
       Expected type: int
       Actual type: ref S
      (15, 8) - (15, 16) Lvalue expected.
      (16, 8) - (16, 24) Type error.
       Expected type: ref int
       Actual type: ref int
      (18, 8) - (18, 18) Type error.
       Expected type: ref int
       Actual type: int
      (20, 8) - (20, 22) Type error.
       Expected type: ref S
       Actual type: S |}]

let%expect_test "RefEqual operator" =
  analyze
    {|
      struct S { int f; ref int rf; };
      ref int ref_int() { return NULL; }
      ref S ref_S() { return NULL; }
      void S::f(ref S other) {
        int a = 1, b = 2;
        ref int ra = a, rb = b;
        S s;
        ra === rb;         // ok
        ra === a;          // ok
        a === ra;          // error: lhs is not a reference
        NULL === ra;       // error: lhs can't be the NULL keyword
        ra === NULL;       // ok
        ra === ref_int();  // ok
        ra === ref_S();    // error: referenced type mismatch
        ref_S() === ra;    // error: referenced type mismatch
        ra === 3;          // error: rhs is not a lvalue
        ref_int() === ra;  // ok
        s.rf === ra;       // ok
        s.f === ra;        // error: lhs is not a reference
        other === this;    // ok
        this === other;    // error: lhs is not a reference
        ref_S() === this;  // ok
        ref_S() === NULL;  // ok
      }
    |};
  [%expect
    {|
      (10, 8) - (10, 16) Type error.
       Expected type: ref int
       Actual type: int
      (11, 8) - (11, 19) Type error.
       Expected type: null
       Actual type: int
      (14, 8) - (14, 22) Type error.
       Expected type: int
       Actual type: ref S
      (15, 8) - (15, 22) Type error.
       Expected type: S
       Actual type: int
      (16, 8) - (16, 16) Lvalue expected.
      (19, 8) - (19, 18) Type error.
       Expected type: ref int
       Actual type: int
      (21, 8) - (21, 22) Lvalue expected. |}]
