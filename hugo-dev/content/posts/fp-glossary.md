---
title: "Functional Programming Glossary"
date: 2018-09-19T12:26:19+02:00
draft: true
---

- **functional programming**: application of the *lambda calculus* to programming.
  - *functions* are regular values. They can be stored in variables and parameters.
- **lambda calculus**: the theoretic foundation of functional programming.
- **type**: A set of values.
  - A value `x` of type `A` is often written `x : A`.
- **function**: Mapping from values of a type `A`, called the *domain*, into another type `B`, called the *codomain*.
  - *Has* to take any value of `A` as input.
  - *Must* only take values of type `A` as input.
  - *Has* to always return a value as output (no exception, no infinite loop).
  - *Must* always return a value of type `B`.
  - *Has* to always return the same output given the same input (i.e. the output only depends on the input!).
  - *Must* satisfy: if `a1 = a2` then `f(a1) = f(a2)` (for a relevant notion of equality).
- **procedures**: they pretend to be functions but have no respect for the rules.
- **type of functions**: functions from type `A` to type `B` are said to be of type `A -> B`.
- **Higher-Order**: functions can take functions as input and return functions as output. Inputs that are functions can also take functions as input and return functions as output. Etc.
- **type constructors**: fonctions taking types as input and returning types as output.
  - Makes generics work.
  - For example `List` is the type function taking any type `A` as input and returning the type `List[A]` as output.
  - In `type F[A] = (A,A)`, `F` is the type function taking any type `X` as input and returning the type of pairs `(X,X)`.
- **Functor**: a type constructor `F` with a function `map` taking a strcuture `(fa : F[A])` and a function `(f : A -> B)` as input and returing a structure `(fb : F[B])`.
  - `map` *must* not alter the value `fa` at all!
  - `fb` *must* have the *exact* same structure as `fa`.
  - every value `(a : A)` in `fa` gives rise to a value `(f(a) : B)` in `fb` at the exact same place with the exact same effects.
- **kind**: the types of types.
  - Basic types `Int`, `String`, `Int -> String`, etc have kind `*` called *star*.
  - Simple type constuctors like `List`, `Option`, etc have kind `* -> *` as they take basic types as input and return basic types as output.
  - More complex type constuctors like *Monad Transformers* have a more complex kind `(* -> *) -> * -> *` as they take other type constructors as input.
- **Kleisli arrow**: a function (on values, not types) of type `A -> F[B]`.
  - For example, `toInt: String -> Option[Int]`.
  - Useful when a direct function `A -> B` is not a good fit for the job. Which integer should `toInt("blabla")` return?