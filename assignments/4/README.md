# VMC Machine for WHILE

Refer to `ParserREADME.md` for changes made in the parser from the previous assignment. Mainly, type-checking and unary operations for constants were now added.

## Usage Instructions

In the current directory, in the terminal run
- run `sml "load.sml"` to load the required signatures into the SML REPL.
- in the REPL, type `run filename` or `debugRun filename` to run the while program.

## Directory Structure

Attached below is the structure of the directory.

- The `src/` directory contains the code of the parser, lexer, their generators, stack, postfix, and VMC definition
    - `stack.sml` this contains the `stack` signature along with `FunStack` structure.
    - `postfix.sml` contains the `postfix` function inside `PostFix` structure.
    - `vmc.sml` contains the `VMC` signature, and `Vmc` structure.
    - `datatypes.sml` contains the `AST` structure along with a `StackElement` datatype.
    - The remaining files are for parsing. They are from the previous assignment barring some modifications.
- Loading `load.sml` generates the parser, lexer, VMC and postfix signature.
- The `tests/` directory contains sample programs in the WHILE language.

```
.
├── load.sml
├── ParserREADME.md
├── README.md
├── src
│   ├── compiler.sml
│   ├── datatypes.sml
│   ├── glue.sml
│   ├── postfix.sml
│   ├── stack.sml
│   ├── vmc.sml
│   ├── while.cm
│   ├── while.lex
│   ├── while.lex.sml
│   ├── while.yacc
│   ├── while.yacc.desc
│   ├── while.yacc.sig
│   └── while.yacc.sml
└── tests
    ├── empty.wh
    ├── expr.wh
    ├── fib.wh
    ├── ite.wh
    ├── loop.wh
    ├── reading.wh
    ├── setting.wh
    └── what.wh
```

## 

## VMC State Transitions

I made some modifications to the transitions of the VMC.

To make the transitions unambiguous, I introduced a new `EOS` symbol, used to represent the end of stack.
- other than the first transition, `C` is either empty or its top element is `EOS`.
- For an expression `e` we denote by `eval(e)` the value we get on evaluating `e`.

```ebnf
<V, M, EOS.C>        -> <V, M, C>
<V, M, e.C>          -> <eval(e).V, M, C>
<V, M, x.e.SET.C>    -> <V, M[x := eval(e)], C>
<V, M, c.d.SEQ.C>    -> <V, M, c.EOS.d.C>
<V, M, b.c.d.ITE.C>  -> <V, M, d.C>              if eval(b) = 0
                     -> <V, M, c.C>              if eval(b) = 1
<V, M, b.c.WH.C>     -> <V, M, C>                if eval(b) = 0
                     -> <V, M, c.EOS.b.c.WH.C>   if eval(b) = 1
<V, M, x.READ.C>     -> <V, M[x := input], C>
<V, M, e.WRITE.C>    -> <V, M, C>                output eval(e)
<V, M, {}.C>         -> <V, M, C>
```

## Signatures
The following signatures are available,

```sml
signature WHILE =
sig
    val compile : string -> AST.Prog
end
```

```sml
signature POSTFIX =
sig
  val postfix : AST.Prog -> StackElement.StackElement FunStack.Stack
  val ast2vmc : AST.Prog -> Vmc.states
end
```

```sml
signature VMC =
sig
  type states = (StackElement.StackElement FunStack.Stack) * (int Array.array)
              * (StackElement.StackElement FunStack.Stack)
  val toString : states -> (string list) * (string list) * (string list)
  val rules    : states -> states
  val execute  : states -> states
end
```

```sml
val run          : string -> Vmc.states
val debugRun     : string -> Vmc.states
val debugExecute : AST.Prog -> Vmc.states
```

- `states` is a tuple `(V, M, C)` where `V`, `C` are `FunStack` and `M` is a mutable `Array`.
- `execute` keeps executing the current state by passing it to `rules` until the control stack becomes empty.
- `postfix` generates the control stack from the AST
- `ast2vmc` generates the VMC state from AST.
- `run` takes in the file name as argument and then executes the WHILE program.
- `debugExecute` does the same as `execute` but also prints the state of the VMC after every transition.
- `debugRun` does the same as `run` but uses `debugExecute` instead of `execute`.

### Other Implementation Decisions

Rather than storing a variable as `VAR of string` on the stacks, I chose to store them as `VAR of int` where each variable was assigned a unique integer, the first variable in the While program was given ID 0, and the subsequent variables were assigned consecutive following values.

## Acknowledgements

- [User’s Guide to ML-Lex and ML-Yacc](http://rogerprice.org/ug/ug.pdf) for explaining the workings of ML-Yacc and ML-Lex.
- `glue.sml`, `compiler.sml`, `while.cm` and boilerplate code for `while.lex` and `while.yacc` were largely created by modifying the `pi` example code given above. The changes made were
	- Changes of instances of `Pi` to `While` in struct, signatures, functions, variables, etc.
	- Other than above, no changes were made to `while.cm`, `glue.sml` and `compiler.sml`.
	- The code in ML-Lex user declarations is largely the same, barring the addition of `overflowError` and replacing the custom hash table with a simple list.
	- While `pi.yacc` was used as the template when writing `while.yacc`, it has been nearly completely rewritten.
