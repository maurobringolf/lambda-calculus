# Haskell in Lambda Calculus

[![test](https://github.com/maurobringolf/lambda-calculus/actions/workflows/test.yml/badge.svg)](https://github.com/maurobringolf/lambda-calculus/actions/workflows/test.yml)

A small subset of Haskell translated into and interpreted as Lambda Calculus (LC), implemented in Haskell.

My curiosity for this project came during a lecture when the professor said "We will implement Haskell in Haskell" but then only presented an LC interpreter in Haskell.
Although LC is only the theoretical base and Haskell implementations use other techniques,
I still wanted to see an actual Haskell interpreter based on LC.
This repository contains my hackery based on this idea:

1. A [parser](https://github.com/maurobringolf/lambda-calculus/blob/master/src/Parser.hs) and [interpreter](https://github.com/maurobringolf/lambda-calculus/blob/master/src/Interpreter.hs) for LC written in Haskell
2. A [compiler](https://github.com/maurobringolf/lambda-calculus/blob/master/src/ChurchEncoding/Compiler.hs) for a subset of Haskell encoding into LC
3. An [interpreter](https://github.com/maurobringolf/lambda-calculus/blob/master/programs/lazy-eval.hs) for LC, written in the Haskell subset supported by (2)

Of course everything is terribly slow and utterly impractical, **BUT** compiling (3) with (2) produces an LC interpreter (the `lazyEval` function) in LC:

```
(λAbs.(λApp.(λVar.(λlength.(λhead.(λmap.(λtake.(λfilter.(λconcat.(λnot.(λdelete.(λdelete.(λelem.(λfresh.(λfreeVars.(λsubst.(λlazyEval.lazyEval)
	((λf.(λx.f (x x)) (λx.f (x x))) (λlazyEval.λe.e (λx.λe.Abs x e) (λe1.λe2.lazyEval e1 (λx.λe.lazyEval (subst x e e2)) (λe1a.λe1b.App e1 e2) (λx.App e1 e2)) (λx.Var x))))
	((λf.(λx.f (x x)) (λx.f (x x))) (λsubst.λx.λe.λt.e (λy.λe2.(λm.λn.(λp.λq.p q p) ((λm.λn.(λn.n (λx.λa.λb.b) (λa.λb.a)) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) m n)) m n) ((λm.λn.(λn.n (λx.λa.λb.b) (λa.λb.a)) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) m n)) n m)) x y (Abs y e2) (elem y (freeVars t) (Abs (fresh (concat (freeVars e2) (freeVars t))) (subst x (subst y e2 (Var (fresh (concat (freeVars e2) (freeVars t))))) t)) (Abs y (subst x e2 t)))) (λe1.λe2.App (subst x e1 t) (subst x e2 t)) (λy.(λm.λn.(λp.λq.p q p) ((λm.λn.(λn.n (λx.λa.λb.b) (λa.λb.a)) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) m n)) m n) ((λm.λn.(λn.n (λx.λa.λb.b) (λa.λb.a)) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) m n)) n m)) x y t (Var y)))))
	((λf.(λx.f (x x)) (λx.f (x x))) (λfreeVars.λe.e (λx.λt.delete x (freeVars t)) (λt1.λt2.concat (freeVars t1) (freeVars t2)) (λx.λc.λn.c x n))))
	(λxs.(λm.λn.λS.λZ.m S (n S Z)) (λS.λZ.S Z) ((λf.λb.λxs.xs f b) (λx.λc.(λn.λm.(λn.n (λx.λa.λb.b) (λa.λb.a)) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) m n)) x c x c) (λS.λZ.Z) xs)))
	(λy.λxs.(λf.λb.λxs.xs f b) (λx.λt.(λp.λq.p p q) t ((λm.λn.(λp.λq.p q p) ((λm.λn.(λn.n (λx.λa.λb.b) (λa.λb.a)) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) m n)) m n) ((λm.λn.(λn.n (λx.λa.λb.b) (λa.λb.a)) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) m n)) n m)) x y)) (λa.λb.b) xs))
	(λx.filter (λy.not ((λm.λn.(λp.λq.p q p) ((λm.λn.(λn.n (λx.λa.λb.b) (λa.λb.a)) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) m n)) m n) ((λm.λn.(λn.n (λx.λa.λb.b) (λa.λb.a)) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) m n)) n m)) y x))))
	((λf.(λx.f (x x)) (λx.f (x x))) (λdelete.λx.λxs.(λm.λn.(λp.λq.p q p) ((λm.λn.(λn.n (λx.λa.λb.b) (λa.λb.a)) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) m n)) m n) ((λm.λn.(λn.n (λx.λa.λb.b) (λa.λb.a)) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) m n)) n m)) (length xs) (λS.λZ.Z) (λc.λn.n) ((λm.λn.(λp.λq.p q p) ((λm.λn.(λn.n (λx.λa.λb.b) (λa.λb.a)) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) m n)) m n) ((λm.λn.(λn.n (λx.λa.λb.b) (λa.λb.a)) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) m n)) n m)) (head xs) x ((λl.λc.λn.l (λh.λt.λg.g h (t c)) (λt.n) (λh.λt.t)) xs) ((λh.λt.λc.λn.c h (t c n)) (head xs) (delete x ((λl.λc.λn.l (λh.λt.λg.g h (t c)) (λt.n) (λh.λt.t)) xs)))))))
	(λb.b (λa.λb.b) (λa.λb.a)))
	((λf.(λx.f (x x)) (λx.f (x x))) (λconcat.λxs.λys.(λm.λn.(λp.λq.p q p) ((λm.λn.(λn.n (λx.λa.λb.b) (λa.λb.a)) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) m n)) m n) ((λm.λn.(λn.n (λx.λa.λb.b) (λa.λb.a)) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) m n)) n m)) (length xs) (λS.λZ.Z) ys ((λh.λt.λc.λn.c h (t c n)) (head xs) (concat ((λl.λc.λn.l (λh.λt.λg.g h (t c)) (λt.n) (λh.λt.t)) xs) ys)))))
	(λf.(λf.λb.λxs.xs f b) (λx.λys.f x ((λh.λt.λc.λn.c h (t c n)) x ys) ys) (λc.λn.n)))
	((λf.(λx.f (x x)) (λx.f (x x))) (λtake.λn.λxs.(λm.λn.(λp.λq.p q p) ((λm.λn.(λn.n (λx.λa.λb.b) (λa.λb.a)) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) m n)) m n) ((λm.λn.(λn.n (λx.λa.λb.b) (λa.λb.a)) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) m n)) n m)) n (λS.λZ.Z) (λc.λn.n) ((λh.λt.λc.λn.c h (t c n)) (head xs) (take ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) n (λS.λZ.S Z)) ((λl.λc.λn.l (λh.λt.λg.g h (t c)) (λt.n) (λh.λt.t)) xs))))))
	(λf.λxs.(λf.λb.λxs.xs f b) (λx.λys.(λh.λt.λc.λn.c h (t c n)) (f x) ys) (λc.λn.n) xs))
	((λf.λb.λxs.xs f b) (λa.λb.a) undefined))
	((λf.λb.λxs.xs f b) (λx.λa.(λm.λn.λS.λZ.m S (n S Z)) a (λS.λZ.S Z)) (λS.λZ.Z)))
	(λx1.λAbs.λApp.λVar.Var x1))
	(λx1.λx2.λAbs.λApp.λVar.App x1 x2))
	(λx1.λx2.λAbs.λApp.λVar.Abs x1 x2)
```

Now this is clearly glorious and was totally worth the effort.

## Example

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

## Usage

```raw
Usage: lambda-calculus-exe [-v|--version] [-e|--eager] [-c|--compileOnly] 
                           [--haskell] FILE [ARGS...]

Available options:
  -h,--help                Show this help text
  -v,--version             Print version and exit.
  -e,--eager               Use eager evaluation strategy instead of lazy (does
                           not apply for --haskell).
  -c,--compileOnly         Do not run the program, but print the lambda term to
                           stdout.
  --haskell                Change input language to Haskell.
  FILE                     Program to execute
  ARGS...                  Optional space-separated list of lambda terms to
                           apply the program to
```

## Untyped λ-calculus

This is the default language used when running the interpreter,
generated by this grammar:

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

The default evaluation strategy used is *call by name*.
The command line argument `--eager` changes this to *call by value*.

## Haskell

Running the interpreter with `--haskell` changes the parsed input language to a subset of Haskell:

* Top level definitions with non-mutual recursion
* A `main` function with result of type `Int`, `Bool` or `List[a]`
* No type annotations, all types are inferred!
* Non-negative integers with `+`, `-`, `*`, `<`, `<=`, `==`
* Booleans with`&&`, `||`
* `if _ then _ else` expressions
* Lambdas, of course
* Lists with `:`, `[_]`, `map`, `length`, `foldr`, `head`, `tail`

The evaluation strategy is lazy, for example the following program makes use of the infinite list `nat` but correctly computes the result `[0,1,2]`:

```hs
natN n = n : natN (n+1)
nat = natN 0
main = take 3 nat
```

For more examples of the supported subset, have a look at `programs/`.

## Encoding

Most of the used encoding is described on the Wikipedia page on [Church Encoding](https://en.wikipedia.org/wiki/Church_encoding).
For the details of the translation see `src/ChurchEncoding/Compiler.hs`.

### Algebraic Data Types

```hs
data List = Cons Int List | Nil

{-
[[Cons]] = (λint.λlist.λCons.λNil.Cons int list)
[[Nil]]  = (λCons.λNil.Nil)

[[case l of Cons i tl -> e1; Nil -> e2; ]] = [[l]] (λi.λtl.[[e1]]) [[e2]]

[[case (Cons 1 Nil) of Cons i tl -> i; Nil -> 0; ]]
  = [[Cons 1 Nil]] (λi.λtl.[[i]]) [[0]]
  = [[Cons 1 Nil]] (λi.λtl.i) 0
  = ((λint.λlist.λCons.λNil.Cons int list) 1 (λCons.λNil.Nil)) (λi.λtl.i) 0
  = (λCons.λNil.Cons 1 (λCons.λNil.Nil)) (λi.λtl.i) 0
  = (λNil.(λi.λtl.i) 1 (λCons.λNil.Nil)) 0
  = (λi.λtl.i) 1 (λCons.λNil.Nil)
  = (λtl.1) 1
  = 1
-}
```

---

## Slightly confusing section

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
