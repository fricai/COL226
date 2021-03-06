# Parser and Lexer for WHILE

This project is a parser and a lexer for the WHILE programming language made using ML-Lex and ML-Yacc.

## Usage Instructions

Start the SML REPL in the current directory,

- run `use "while_ast.sml";` to generate the lexer and parser, and load the compiler structure into the SML REPL.
- run `While.compile <filepath>;` to create the AST from the WHILE language code stored in `<filepath>`.

## Directory Structure

Attached below is the structure of the directory.

- The `src/` directory contains the code of the parser, lexer, and their generators.
	- `while.cm` is the SML Compilation Manager file used to easily load the code.
	- `datatypes.sml` defines the data type of the AST generated.
	- `while.lex` is the ML-Lex code used for generating the lexer.
	- `while.yacc` is the ML-Yacc code used for generating the parser.
	- `glue.sml` is glue code for running ML-Lex and ML-Yacc with each other.
	- `compiler.sml` defines a structure which uses the parser and lexer generated to construct the AST from a file in WHILE language.
	- `while.yacc.desc` is a debugging file containing a description of the LR parser generated by ML-Yacc.
	- `while.yacc.sig` and `while.yacc.sml` define the parser generated by ML-Yacc.
	- `while.lex.sml` defines the lexer generated by ML-Lex.
- Loading `while_ast.sml` generates the parser and lexer.
- The `tests/` directory contains sample programs in the WHILE language.

## Context-Free Grammar
The CFG generating the language is given by,
```ebnf
begin          = "program" identifier "::" block
block          = declarationseq commandseq
declarationseq = {declaration}
declaration    = "var" varlist ":" type
type           = "int" | "bool"
varlist        = variable {"," variable}
variable       = identifier
commandseq     = "{" {command ";"} "}"
command        = variable ":=" expression
               | "read" variable
	       | "write" expression
	       | "if" expression "then" commandseq "else" commandseq "endif"
	       | "while" expression "do" commandseq "endwh"
expression     = expression addop expression
               | expression boolop expression
	       | expression mulop expression
	       | expression relop expression
	       | "(" expression ")"
	       | unaryop expression
               | integer
	       | "tt" | "ff"
integer        = digit{digit}
identifier     = letter{letter | digit}
unaryop        = "~" | "!"
mulop          = "*" | "/" | "%"
addop          = "+" | "-"
relop          = "<" | "<=" | "=" | "<>" | ">=" | ">"
boolop         = "&&" | "||"
```

where `digit` and `letter` are non-terminals which derive the digits `0` to `9` and the alphabetical characters `a` to `z` and `A` to `Z` respectively.

The CFG as defined above is ambiguous because of the (lack of) associativity and precedence of operations. These operators are, in order of _decreasing_ precedence.

| Operation      | Symbols                         | Associativity   |
| -------------- | ------------------------------- | --------------- |
| Unary          | `!`, `~`                        | right           |
| Multiplicative | `*`, `/`, `%`                   | left            |
| Additive       | `+`, `-`                        | left            |
| Relational     | `<`, `<=`, `=`, `<>`, `>=`, `>` | non-associative |
| Boolean AND    | `&&`                            | left            |
| Boolean OR     | `||`                            | left            |

## AST datatype definition

```sml
structure AST =
struct
  type Var = string
  datatype
      Prog   = PROG     of string * Blk
  and Blk    = BLK      of (Dec list) * (Cmd list)
  and Dec    = INT      of Var
             | BOOL     of Var
  and Cmd    = SET      of Var * Exp
             | READ     of Var
             | WRITE    of Exp
             | ITE      of Exp * (Cmd list) * (Cmd list)
             | WH       of Exp * (Cmd list)
  and Exp    = (* boolean operators *)
               AND      of Exp * Exp
             | OR       of Exp * Exp
             | NOT      of Exp
             (* relational operators *)
             | LT       of Exp * Exp
             | LEQ      of Exp * Exp
             | EQ       of Exp * Exp
             | GT       of Exp * Exp
             | GEQ      of Exp * Exp
             | NEQ      of Exp * Exp
             (* integer operators *)
             | PLUS     of Exp * Exp
             | MINUS    of Exp * Exp
             | TIMES    of Exp * Exp
             | DIV      of Exp * Exp
             | MOD      of Exp * Exp
             | NEGATIVE of Exp
             (* terminals *)
             | VAR      of Var
             | INTVAL   of int
             | BOOLVAL  of bool
end;
```

## Syntax-directed translation

```ebnf
begin          = "program" identifier "::" block => AST.PROG(identifier, block)
block          = declarationseq commandseq       => AST.BLK(declarationseq, commandseq)
declarationseq = {declaration}                   => flattened list of declarations
declaration    = "var" varlist ":" type          => list of type(variable)
type           = "int"                           => AST.INT
               | "bool"                          => AST.BOOL
varlist        = variable {"," variable}         => list of variables
variable       = identifier                      => identifier
commandseq     = "{" {command ";"} "}"           => list of commands
command        = variable ":=" expression        => AST.SET(variable, expression)
               | "read" variable                 => AST.READ(variable)
               | "write" expression              => AST.WRITE(expression)
               | "if" expression "then" commandseq1
                 "else" commandseq2 "endif"      => AST.ITE(expression, commandseq1, commandseq2)
               | "while" expression "do" commandseq
                 "endwh"                         => AST.WH(expression, commandseq)
expression     = expression1 addop expression2   => addop(expression1, expression2)
               | expression1 boolop expression2  => boolop(expression1, expression2)
               | expression1 mulop expression2   => mulop(expression1, expression2)
               | expression1 relop expression2   => relop(expression1, expression2)
               | "(" expression ")"              => expression
               | unaryop expression              => unaryop(expression)
               | variable                        => AST.VAR(variable)
               | integer                         => AST.INTVAL integer
               | "tt"                            => AST.BOOLVAL true
               | "ff"                            => AST.BOOLVAL false
integer        = [+|~]digit{digit}               => int representing the value
identifier     = letter{letter | digit}          => string of letters and digits
unaryop        = "~"                             => AST.NEGATIVE
               | "!"                             => AST.NOT
mulop          = "*"                             => AST.TIMES
               | "/"                             => AST.DIV
               | "%"                             => AST.MOD
addop          = "+"                             => AST.PLUS
               | "-"                             => AST.MINUS
relop          = "<"                             => AST.LT
               | "<="                            => AST.LEQ
               | "="                             => AST.EQ
               | "<>"                            => AST.NEQ
               | ">="                            => AST.GEQ
               | ">"                             => AST.GT
boolop         = "&&"                            => AST.AND
               | "||"                            => AST.OR
```

### Auxiliary functions and Data

Along with the datatypes and constructors defined in the problem statement.
I made the following changes to the AST,

- A `type Var = string` to make the code easier to read.
- A singular datatype `Exp` rather than separate `IEXP` and `BEXP` constructors. Its constructors are those specified in the problem statement, along with the additional constructors,
	- `NEGATIVE` to represent the unary minus.
	- `VAR` to represent a terminal node containing a variable.
	- `INTVAL` to represent a terminal node containing a constant integer.
	- `BOOLVAL` to represent a terminal node containing a constant boolean.
- A command or declaration list was used for sequencing instead of the constructor `SEQ`.
- `TT` and `FF` were no longer needed due to `BOOLVAL`.

## Other Design Decisions

My design majorly differs from the syntax as specified by the EBNF in the following ways:

1. Relational operators make sense for boolean expressions too, as specified in the problem statement.
2. Boolean and integer expressions are no longer separate terminals.
3. Unary plus is not supported, due to ambiguities in the grammar and no reasonable way to resolve this in ML-Yacc.
4. Relational operators are _not_ associative so `A < B < C` when `A`, `B` are `int` and `C` is `bool` is _not_ evaluated as `(A < B) < C`, rather, an error is thrown.

## Other Implementation Decisions

I decided to use a KeyWords structure containing a list of keywords in the ML-Lex file to make it easier to work with keywords.

The program uses an SML Compilation Manager file, `while.cm`, to make it easier to load all the required files.

## Acknowledgements

- [User???s Guide to ML-Lex and ML-Yacc](http://rogerprice.org/ug/ug.pdf) for explaining the workings of ML-Yacc and ML-Lex.
- `glue.sml`, `compiler.sml`, `while.cm` and boilerplate code for `while.lex` and `while.yacc` were largely created by modifying the `pi` example code given above. The changes made were
	- Changes of instances of `Pi` to `While` in struct, signatures, functions, variables, etc.
	- Other than above, no changes were made to `while.cm`, `glue.sml` and `compiler.sml`.
	- The code in ML-Lex user declarations is largely the same, barring the addition of `overflowError` and replacing the custom hash table with a simple list.
	- While `pi.yacc` was used as the template when writing `while.yacc`, it has been nearly completely rewritten.
