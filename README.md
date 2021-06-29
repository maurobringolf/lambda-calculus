# Haskell in Lambda Calculus

[![test](https://github.com/maurobringolf/lambda-calculus/actions/workflows/test.yml/badge.svg)](https://github.com/maurobringolf/lambda-calculus/actions/workflows/test.yml)

A small subset of Haskell translated into and interpreted as Lambda Calculus, implemented in Haskell.
I know that real Haskell is not run this way and this is just a personal study project.

As a first example, this program:

```hs
main = map (\x -> x * 2) [1,2,3]
```

Is parsed, typechecked and compiled to (`;` indicates a comment):

```lisp
(λlength.
  (λhead.
    (λtail.
      (λmap.
        map
          ; \x -> x * 2
          (λx.(λm.λn.m ((λm.λn.λS.λZ.m S (n S Z)) n) (λS.λZ.Z)) x (λS.λZ.S (S Z)))
          ; [1,2,3]
          (λc.λn.c (λS.λZ.S Z) (c (λS.λZ.S (S Z)) (c (λS.λZ.S (S (S Z))) n)))
        )
      ; map
      (λf.λxs.(λf.λb.λxs.xs f b) (λx.λys.(λh.λt.λc.λn.c h (t c n)) (f x) ys) (λc.λn.n) xs)
    )
    ; tail
    (λxs.λc.λn.(λf.λb.λxs.xs f b) (λh.λt.λg.g h (t c)) (λt.n) xs (λh.λt.t))
  )
  ; head
  ((λf.λb.λxs.xs f b) (λa.λb.a) undefined)
)
; length
((λf.λb.λxs.xs f b) (λx.λa.(λm.λn.λS.λZ.m S (n S Z)) a (λS.λZ.S Z)) (λS.λZ.Z))
```

Which is then evaluated and interpreted according to its type `[Int]`:

```
[2,4,6]
```

If you are confused, don't read the [slightly more confusing section](slightly-more-confusing-section).

## Usage

```raw
Usage: lambda-calculus-exe [-v|--version] [-e|--eager] [-c|--compileOnly] 
                           [-s|--syntaxSugar] FILE [ARGS...]

Available options:
  -h,--help                Show this help text
  -v,--version             Print version and exit.
  -e,--eager               Use eager evaluation strategy instead of lazy (the
                           default).
  -c,--compileOnly         Parse and compile only and print the result to
                           stdout.
  -s,--syntaxSugar         Compile away all syntax sugar.
  FILE                     Program to execute
  ARGS...                  Optional space-separated list of lambda terms to
                           apply the program to
```

## Untyped Lambda Calculus

This is the default language used when running the interpreter.

### Syntax

```
t ::= X
    | t t
    | λX.t
```

Standard syntactial conventions are used: 

* `X` is a meta-variable and stands in for the class of alpha-numeric strings
* Application associates to the left: `t1 t2 t3 = (t1 t2) t3`
* Application binds stronger than abstraction: `λX.t1 t2 = λX.(t1 t2)`
* Parenthesis are used for grouping

### Semantics

The default evaluation strategy used is *call by name*.
The command line argument `--eager` changes this to *call by value*.

## Haskell

Running the interpreter with `--haskell` changes the input language to a subset of Haskell:

* Top level definitions with non-mutual recursion
* Non-negative integers

### Language Support

Haskell syntax of the following language constructs is supported:

* Top level definitions

## Encoding

Most of the used encoding is described on the Wikipedia page on [Church Encoding](https://en.wikipedia.org/wiki/Church_encoding).

## Slightly more confusing section

Since Lambda Calculus is a subset of Haskell, we can lift compiled programs back into Haskell.
The result of this procedure is a hopefully equivalent, larger, slower and rather unreadable version of the input program.
Take the same example as above:

```hs
main = map (\x -> x * 2) [1,2,3]
```

Which, after compiling and lifting, becomes:

```hs
(\length -> 
  (\head -> 
    (\tail -> 
      (\map -> 
        map
          (\x -> (\m -> \n -> m ((\m -> \n -> \s -> \z -> m s (n s z)) n) (\s -> \z -> z)) x (\s -> \z -> s (s z)))
          (\c -> \n -> c (\s -> \z -> s z) (c (\s -> \z -> s (s z)) (c (\s -> \z -> s (s (s z))) n)))
        )
      (\f -> \xs -> (\f -> \b -> \xs -> xs f b) (\x -> \ys -> (\h -> \t -> \c -> \n -> c h (t c n)) (f x) ys) (\c -> \n -> n) xs)
    )
    (\xs -> \c -> \n -> (\f -> \b -> \xs -> xs f b) (\h -> \t -> \g -> g h (t c)) (\t -> n) xs (\h -> \t -> t))
  )
  ((\f -> \b -> \xs -> xs f b) (\a -> \b -> a) undefined)
  ) ((\f -> \b -> \xs -> xs f b) (\x -> \a -> (\m -> \n -> \s -> \z -> m s (n s z)) a (\s -> \z -> s z)) (\s -> \z -> z))
```

When loading this into `ghci` it does not evaluate to `[2,4,6]`, because it is now Church encoded on the Haskell level. So we need an extra decoding step:

```
*Main> xs = ... all of the stuff above ...
*Main> xs
<interactive>:19:1: error:
    • No instance for (Show
                         ((((p10 -> p10) -> p10 -> p10) -> p20 -> p20) -> p20 -> p20))
        arising from a use of ‘print’
        (maybe you haven't applied a function to enough arguments?)
    • In a stmt of an interactive GHCi command: print it
*Main> map (\n -> n (+ 1) 0) (xs (:) [])
[2,4,6]
```
