# Lambda Calculus

[![test](https://github.com/maurobringolf/lambda-calculus/actions/workflows/test.yml/badge.svg)](https://github.com/maurobringolf/lambda-calculus/actions/workflows/test.yml)

An interpreter for the untyped lambda calculus.

```raw
Usage: lc [-v|--version] [-e|--eager] FILE [ARGS...]

Available options:
  -h,--help                Show this help text
  -v,--version             Print version and exit.
  -e,--eager               Use eager evaluation strategy instead of lazy (the
                           default).
  FILE                     Program to execute
  ARGS...                  Optional space-separated list of lambda terms to
                           apply the program to
```

## Syntax

```
t ::= X
    | t t
    | λX.t
```

Standard syntactial conventions are used: 

* `X` is a meta-variable and stands in for the class of alpha-numeric strings
* Application associates to the left: `t1 t2 t3 = (t1 t2) t3`
* Abstraction associates to the right: `λX.λY.t = λX.(λY.t)`
* Application binds stronger than abstraction: `λX.t1 t2 = λX.(t1 t2)`

## Example

Assuming that the file `add.la` contains the addition function for [Church numerals](https://en.wikipedia.org/wiki/Church_encoding),
here is how `1 + 2` can be computed from the command line:

```
$> cat add.la
λm.λn.λf.λx.m f (n f x)
$> lc -e add.la "λf.λx.f x" "λf.λx.f (f x)"
λf.λx.(λf.λx.f x) f ((λf.λx.f (f x)) f x)
$> lc -e add.la "λf.λx.f x" "λf.λx.f (f x)"
λf.λx.f (f (f x))
```

However, passing arguments via arguments is just for convenience and they can be included in the program as well:

```
$> cat add_one_two.la
(λm.λn.λf.λx.m f (n f x)) (λf.λx.f x) (λf.λx.f (f x))
$> lc -e add_one_two.la 
λf.λx.(λf.λx.f x) f ((λf.λx.f (f x)) f x)
$> lc -e add_one_two.la
λf.λx.f (f (f x))
```
