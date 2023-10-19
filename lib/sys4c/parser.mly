(* Copyright (C) 2021 Nunuhara Cabbage <nunuhara@haniwa.technology>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://gnu.org/licenses/>.
 *)

%{

open Jaf

let qtype qualifier data =
  { data=data; qualifier=qualifier }

let expr loc ast =
  { valuetype=None; node=ast; loc }

let stmt loc ast =
  { node=ast; loc }

type varinit = {
  name: string;
  loc: Lexing.position * Lexing.position;
  dims: expression list;
  initval: expression option;
}

let decl typespec vi =
  { name=vi.name; location=vi.loc; array_dim=vi.dims; type_spec=typespec; initval=vi.initval; index=None }

let decls typespec var_list =
  List.map (decl typespec) var_list

let func loc typespec name params body =
  (* XXX: hack for `functype name(void)` *)
  let plist =
    match params with
    | [{ type_spec={data=Void; _}; _}] -> []
    | _ -> params
  in
  { name; loc; struct_name=None; return=typespec; params=plist; body; is_label=false; index=None; class_index=None; super_index=None }

let member_func loc typespec_opt struct_name is_dtor name params body =
  let name = if is_dtor then "~" ^ name else name in
  let fundecl = func loc (Option.value typespec_opt ~default:(qtype None Void)) name params body in
  { fundecl with struct_name=Some struct_name }

%}

%token <int> I_CONSTANT
%token <float> F_CONSTANT
%token <string> C_CONSTANT
%token <string> S_CONSTANT
%token <string> IDENTIFIER
/* arithmetic */
%token PLUS MINUS TIMES DIV MOD
/* bitwise */
%token LSHIFT RSHIFT BITAND BITOR BITXOR
/* logic/comparison */
%token AND OR LT GT LTE GTE EQUAL NEQUAL REF_EQUAL REF_NEQUAL
/* unary */
%token INC DEC BITNOT LOGNOT
/* assignment */
%token ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN MODULOASSIGN
%token ORASSIGN XORASSIGN ANDASSIGN LSHIFTASSIGN RSHIFTASSIGN REFASSIGN
%token SWAP
/* delimiters */
%token LPAREN RPAREN RBRACKET LBRACKET LBRACE RBRACE
%token QUESTION COLON SEMICOLON COCO AT COMMA DOT HASH
/* types */
%token VOID CHAR INT LINT FLOAT BOOL STRING HLL_STRUCT HLL_PARAM HLL_FUNC HLL_DELEGATE
%token IMAINSYSTEM
/* keywords */
%token IF ELSE WHILE DO FOR SWITCH CASE DEFAULT NULL THIS NEW
%token GOTO CONTINUE BREAK RETURN
%token CONST REF OVERRIDE ARRAY WRAP FUNCTYPE DELEGATE STRUCT CLASS PRIVATE PUBLIC ENUM

%token EOF

%nonassoc IFX
%nonassoc ELSE

%start jaf
%type <declaration list> jaf

%start hll
%type <declaration list> hll

%%

jaf
  : external_declaration+ EOF { List.concat $1 }
  ;

hll
  : hll_declaration+ EOF { List.concat $1 }
  ;

primary_expression
  : IDENTIFIER { expr $sloc (Ident ($1, None)) }
  | THIS { expr $sloc This }
  | NULL { expr $sloc Null }
  | constant { expr $sloc $1 }
  | string { expr $sloc $1 }
  | LPAREN expression RPAREN { {$2 with loc=$sloc} }
  ;

constant
  : I_CONSTANT { ConstInt ($1) }
  | C_CONSTANT { ConstChar ($1) }
  | F_CONSTANT { ConstFloat ($1) }
  (* E_CONSTANT *)
  ;

string
  : S_CONSTANT { ConstString ($1) }
  (* FILE_MACRO *)
  (* LINE_MACRO *)
  (* FUNC_MACRO *)
  (* DATE_MACRO *)
  (* TIME_MACRO *)
  ;

postfix_expression
  : primary_expression { $1 }
  | postfix_expression LBRACKET expression RBRACKET { expr $sloc (Subscript ($1, $3)) }
  | atomic_type_specifier LPAREN expression RPAREN { expr $sloc (Cast ($1, $3)) }
  | postfix_expression arglist { expr $sloc (Call ($1, $2, None)) }
  | NEW IDENTIFIER arglist { expr $sloc (New (Unresolved ($2), $3, None)) }
  | postfix_expression DOT IDENTIFIER { expr $sloc (Member ($1, $3, None)) }
  | postfix_expression INC { expr $sloc (Unary (PostInc, $1)) }
  | postfix_expression DEC { expr $sloc (Unary (PostDec, $1)) }
  ;

arglist: LPAREN separated_list(COMMA, assign_expression) RPAREN { $2 }

unary_expression
  : postfix_expression { $1 }
  | INC unary_expression { expr $sloc (Unary (PreInc, $2)) }
  | DEC unary_expression { expr $sloc (Unary (PreDec, $2)) }
  | unary_operator cast_expression { expr $sloc (Unary ($1, $2)) }
  ;

unary_operator
  : PLUS { UPlus }
  | MINUS { UMinus }
  | BITNOT { BitNot }
  | LOGNOT { LogNot }
  | BITAND { AddrOf }
  ;

cast_expression
  : unary_expression { $1 }
  | LPAREN atomic_type_specifier RPAREN cast_expression { expr $sloc (Cast ($2, $4)) }
  ;

mul_expression
  : cast_expression { $1 }
  | mul_expression TIMES cast_expression { expr $sloc (Binary (Times, $1, $3)) }
  | mul_expression DIV cast_expression { expr $sloc (Binary (Divide, $1, $3)) }
  | mul_expression MOD cast_expression { expr $sloc (Binary (Modulo, $1, $3)) }
  ;

add_expression
  : mul_expression { $1 }
  | add_expression PLUS mul_expression { expr $sloc (Binary (Plus, $1, $3)) }
  | add_expression MINUS mul_expression { expr $sloc (Binary (Minus, $1, $3)) }
  ;

shift_expression
  : add_expression { $1 }
  | shift_expression LSHIFT add_expression { expr $sloc (Binary (LShift, $1, $3)) }
  | shift_expression RSHIFT add_expression { expr $sloc (Binary (RShift, $1, $3)) }
  ;

rel_expression
  : shift_expression { $1 }
  | rel_expression LT shift_expression { expr $sloc (Binary (LT, $1, $3)) }
  | rel_expression GT shift_expression { expr $sloc (Binary (GT, $1, $3)) }
  | rel_expression LTE shift_expression { expr $sloc (Binary (LTE, $1, $3)) }
  | rel_expression GTE shift_expression { expr $sloc (Binary (GTE, $1, $3)) }
  ;

eql_expression
  : rel_expression { $1 }
  | eql_expression EQUAL rel_expression { expr $sloc (Binary (Equal, $1, $3)) }
  | eql_expression NEQUAL rel_expression { expr $sloc (Binary (NEqual, $1, $3)) }
  | eql_expression REF_EQUAL rel_expression { expr $sloc (Binary (RefEqual, $1, $3)) }
  | eql_expression REF_NEQUAL rel_expression { expr $sloc (Binary (RefNEqual, $1, $3)) }
  ;

and_expression
  : eql_expression { $1 }
  | and_expression BITAND eql_expression { expr $sloc (Binary (BitAnd, $1, $3)) }
  ;

xor_expression
  : and_expression { $1 }
  | xor_expression BITXOR and_expression { expr $sloc (Binary (BitXor, $1, $3)) }
  ;

ior_expression
  : xor_expression { $1 }
  | ior_expression BITOR xor_expression { expr $sloc (Binary (BitOr, $1, $3)) }
  ;

logand_expression
  : ior_expression { $1 }
  | logand_expression AND ior_expression { expr $sloc (Binary (LogAnd, $1, $3)) }
  ;

logor_expression
  : logand_expression { $1 }
  | logor_expression OR logand_expression { expr $sloc (Binary (LogOr, $1, $3)) }
  ;

cond_expression
  : logor_expression { $1 }
  | logor_expression QUESTION expression COLON cond_expression { expr $sloc (Ternary ($1, $3, $5)) }
  ;

assign_expression
  : cond_expression { $1 }
  | unary_expression assign_operator assign_expression { expr $sloc (Assign ($2, $1, $3)) }
  ;

assign_operator
  : ASSIGN       { EqAssign }
  | PLUSASSIGN   { PlusAssign }
  | MINUSASSIGN  { MinusAssign }
  | TIMESASSIGN  { TimesAssign }
  | DIVIDEASSIGN { DivideAssign }
  | MODULOASSIGN { ModuloAssign }
  | ORASSIGN     { OrAssign }
  | XORASSIGN    { XorAssign }
  | ANDASSIGN    { AndAssign }
  | LSHIFTASSIGN { LShiftAssign }
  | RSHIFTASSIGN { RShiftAssign }
  ;

expression
  : assign_expression { $1 }
  | expression COMMA assign_expression { expr $sloc (Seq ($1, $3)) }
  ;

constant_expression
  : cond_expression { $1 }
  ;

atomic_type_specifier
  : VOID         { Void }
  | CHAR         { Int }
  | INT          { Int }
  | LINT         { Int }
  | FLOAT        { Float }
  | BOOL         { Bool }
  | STRING       { String }
  | HLL_STRUCT   { Struct("hll_struct", -1) }
  | HLL_PARAM    { HLLParam }
  | HLL_FUNC     { HLLFunc }
  | HLL_DELEGATE { Delegate("hll_delegate", -1) }
  | IMAINSYSTEM  { IMainSystem }
  ;

type_qualifier
  : CONST { Const }
  | REF { Ref }
  | OVERRIDE { Override }
  ;

type_specifier
  : atomic_type_specifier { $1 }
  (* FIXME: this disallows arrays/wraps of ref-qualified types *)
  | ARRAY AT type_specifier { Array (qtype None $3) }
  | WRAP AT type_specifier { Wrap (qtype None $3) }
  | IDENTIFIER { Unresolved ($1) }

statement
  : declaration_statement { stmt $sloc $1 }
  | labeled_statement { stmt $sloc $1 }
  | compound_statement { stmt $sloc $1 }
  | expression_statement { stmt $sloc $1 }
  | selection_statement { stmt $sloc $1 }
  | iteration_statement { stmt $sloc $1 }
  | jump_statement { stmt $sloc $1 }
  | message_statement { stmt $sloc $1 }
  | rassign_statement { stmt $sloc $1 }
  | objswap_statement { stmt $sloc $1 }
  ;

switch_statement
  : CASE constant_expression COLON switch_statement { stmt $sloc (Case ($2, $4)) }
  | DEFAULT COLON switch_statement { stmt $sloc (Default ($3)) }
  | statement { $1 }
  ;

declaration_statement
  : declaration { Declarations $1 }

labeled_statement
  : IDENTIFIER COLON statement { Labeled ($1, $3) }
  (* case *)
  (* default *)
  ;

compound_statement
  : block { match $1 with [] -> EmptyStatement | _ -> Compound $1 }
  ;

block
  : LBRACE nonempty_list(statement) RBRACE { $2 }
  | LBRACE RBRACE { [] }
  ;

expression_statement
  : SEMICOLON { EmptyStatement }
  | expression SEMICOLON { Expression ($1) }
  ;

selection_statement
  : IF LPAREN expression RPAREN statement %prec IFX
    { If ($3, $5, stmt ($endpos, $endpos) EmptyStatement) }
  | IF LPAREN expression RPAREN statement ELSE statement
    { If ($3, $5, $7) }
  | SWITCH LPAREN expression RPAREN LBRACE switch_statement+ RBRACE
    { Switch ($3, $6) }
  ;

iteration_statement
  : WHILE LPAREN expression RPAREN statement { While ($3, $5) }
  | DO statement WHILE LPAREN expression RPAREN { DoWhile ($5, $2) }
  | FOR LPAREN expression_statement expression? SEMICOLON expression? RPAREN statement
    { For (stmt $loc($3) $3,
           $4,
           $6,
           $8)
    }
  | FOR LPAREN declaration expression? SEMICOLON expression? RPAREN statement
    { For (stmt $loc($3) (Declarations $3),
           $4,
           $6,
           $8)
    }
  ; 

jump_statement
  : GOTO IDENTIFIER SEMICOLON { Goto ($2) }
  | CONTINUE SEMICOLON { Continue }
  | BREAK SEMICOLON { Break }
  | RETURN expression? SEMICOLON { Return ($2) }
  ;

message_statement
  : C_CONSTANT { MessageCall $1 }
  ;

rassign_statement
  : expression REFASSIGN expression SEMICOLON { RefAssign ($1, $3) }

objswap_statement
  : expression SWAP expression SEMICOLON { ObjSwap ($1, $3) }

declaration
  : declaration_specifiers separated_nonempty_list(COMMA, init_declarator) SEMICOLON
    { decls $1 $2 }
  ;

declaration_specifiers
  : type_qualifier type_specifier { qtype (Some $1) $2 }
  | type_specifier { qtype None $1 }
  ;

init_declarator
  : declarator ASSIGN assign_expression { { $1 with initval = Some $3; loc = $sloc } }
  | declarator { $1 }
  ;

declarator
  : IDENTIFIER { { name=$1; dims=[]; initval=None; loc=$sloc } }
  | array_allocation { $1 }
  ;

array_allocation
  : IDENTIFIER LBRACKET expression RBRACKET { { name=$1; loc=$sloc; initval=None; dims=[$3] } }
  | array_allocation LBRACKET expression RBRACKET
    { { $1 with dims = $3 :: $1.dims; loc = $sloc } }
  ;

external_declaration
  : declaration
    { List.map (fun d -> Global (d)) $1 }
  | declaration_specifiers IDENTIFIER parameter_list block
    { [Function (func $sloc $1 $2 $3 (Some $4))] }
  | ioption(declaration_specifiers) IDENTIFIER COCO boption(BITNOT) IDENTIFIER parameter_list block
    { [Function (member_func $sloc $1 $2 $4 $5 $6 (Some $7))] }
  | HASH IDENTIFIER parameter_list block
    { [Function { (func $sloc (qtype None Void) $2 $3 (Some $4)) with is_label=true }] }
  | FUNCTYPE declaration_specifiers IDENTIFIER functype_parameter_list SEMICOLON
    { [FuncTypeDef (func $sloc $2 $3 $4 None)] }
  | DELEGATE declaration_specifiers IDENTIFIER functype_parameter_list SEMICOLON
    { [DelegateDef (func $sloc $2 $3 $4 None)] }
  | struct_or_class IDENTIFIER LBRACE struct_declaration+ RBRACE SEMICOLON
    { [StructDef ({ loc=$sloc; is_class=$1; name=$2; decls=(List.concat $4) })] }
  | ENUM enumerator_list SEMICOLON
    { [Enum ({ loc=$sloc; name=None; values=$2 })] }
  | ENUM IDENTIFIER enumerator_list SEMICOLON
    { [Enum ({ loc=$sloc; name=Some $2; values=$3 })] }
  ;

struct_or_class
  : STRUCT { false }
  | CLASS { true }
  ;

hll_declaration
  : declaration_specifiers IDENTIFIER parameter_list SEMICOLON
    { [Function (func $sloc $1 $2 $3 None)] }
  | struct_or_class IDENTIFIER LBRACE struct_declaration+ RBRACE SEMICOLON
    { [StructDef ({ loc=$sloc; is_class=$1; name=$2; decls=(List.concat $4) })] }
  ;

enumerator_list
  : LBRACE separated_nonempty_list(COMMA, enumerator) RBRACE { $2 }
  ;

enumerator
  : IDENTIFIER ASSIGN constant_expression { ($1, Some $3) }
  | IDENTIFIER { ($1, None) }
  ;

parameter_declaration
  : declaration_specifiers declarator { decl $1 { $2 with loc=$sloc } }
  ;

parameter_list
  : LPAREN separated_list(COMMA, parameter_declaration) RPAREN { $2 }
  | LPAREN VOID RPAREN { [] }
  ;

functype_parameter_declaration
  : declaration_specifiers { decl $1 { name="<anonymous>"; dims=[]; initval=None; loc=$sloc } }
  | parameter_declaration { $1 }
  ;

functype_parameter_list
  : LPAREN separated_list(COMMA, functype_parameter_declaration) RPAREN { $2 }
  ;

struct_declaration
  : access_specifier COLON
    { [AccessSpecifier $1] }
  | declaration_specifiers separated_nonempty_list(COMMA, declarator) SEMICOLON
    { decls $1 $2 |> List.map (fun d -> MemberDecl (d)) }
  | declaration_specifiers IDENTIFIER parameter_list opt_body
    { [Method (func $sloc $1 $2 $3 $4)] }
  | IDENTIFIER LPAREN RPAREN opt_body
    { [Constructor (func $sloc {data=Void; qualifier=None} $1 [] $4)] }
  | BITNOT IDENTIFIER LPAREN RPAREN opt_body
    { [Destructor (func $sloc {data=Void; qualifier=None} $2 [] $5)] }
  ;

opt_body
  : SEMICOLON { None }
  | block { Some $1 }
  ;

access_specifier
  : PUBLIC { Public }
  | PRIVATE { Private }
  ;
