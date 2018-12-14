---
title: "Episode 4 - CoProducts"
date: 2018-12-16T00:00:00+04:00
draft: false
type: "types"
episode: 4
episodeAsString: "fourth"
---

CoProducts, often called *sum types*, *discriminated unions* or *disjoint unions*, are a convenient way to express an alternative between different types. The **coproduct** of `n` types (with `n` being *0*, *1*, *2*, etc) `A_1`, `A_2`, ..., `A_n` is precisely the type whose values are `(i, a_i)` where `i` is a number, called the **tag**, between `1` and `n` both included (`1 ≤ i ≤  n`) and `a_i` is a value of type `A_i` (the actual type then depends on the value of `i`). Is is often written `Either[A_1, ..., A_n]` in programming and `A_1 + ... + A_n` in *Type Theory*.

Let's take, as an example, `n = 4`, `A_1 = Char`, `A_2 = Boolean`, `A_3 = Char` and `A_4 = Double`. The following values are all **valid and distinct** values of type `Either4[Char, Boolean, Char, Double]`: `(1, 'C')`, `(2, true)`, `(3, 'D')`, `(4, 3.2)`, `(3, 'C')`. Note that `(1, 'C')` and `(3, 'C')` are different values because the *tag* is different.

Instead of writing the *tag* explicitly, programming languages often let us write `n` **constructors**: `Inj_i: A_i => Either_n[A_1, ..., A_n]` such that
$$\forall i\in{1,\dots,n},\quad \texttt{Inj}_i : \texttt{A}_i \rightarrow \texttt{Either}_n[\texttt{A}_1,\dots,\texttt{A}_n]$$
$$\forall i\in{1,\dots,n},\quad \forall a_i:\texttt{A}_i,\quad \texttt{Inj}_i(a_i) = (i, a_i)$$

Note that:

- As usual **constructors are injective** functions
  $$\forall i\in{1,\dots,n},\quad \forall a_i,a'_i:\texttt{A}_i,\quad \texttt{Inj}_i(a_i) = \texttt{Inj}_i(a'_i) \Leftrightarrow a_i = a'_i$$
- The **only way to get a value** of type `Either_n[A_1, ... , A_n]` is by using one of the constructors
  $$\forall v:\texttt{Either}_n[\texttt{A}_1,\dots,\texttt{A}_n],\quad \exists! i\in{1,\dots,n},\quad \texttt{Inj}_i(a_i) = v$$
- Two **different constructors produce different values** (hense the *disjoint* and *discriminated* unions)
  $$\forall i,j\in{1,\dots,n},\quad \forall a_i:\texttt{A}_i,\quad \forall a_j:\texttt{A}_j,\quad \texttt{Inj}_i(a_i) = \texttt{Inj}_j(a_j) \Leftrightarrow i = j \textbf{ and } a_i = a_j$$

For example with `n = 2`, the *coproduct* of two types `A` and `B` is defined as

```scala
sealed abstract class Either2[A,B] {
  def fold[R](inj1: A => R, inj2: B => R): R =
    this match {
      case Inj1(a) => inj1(a)
      case Inj2(b) => inj2(b)
    }
}
final case class Inj1[A,B](value: A) extends Either2[A,B]
final case class Inj2[A,B](value: B) extends Either2[A,B]
```

Saying the pattern matching above is exhaustive is exactly equivalent to say a value of type `Either2[A,B]` has to be either `Inj1(a)` for some `a:A` or `Inj2(b)` for some `b:B`. Requiring that `Inj1` and `Inj2` produce different values is also mandatory as `Eirher2[Error, Result]` is often used to model computation that may fail. A failed computation would produce a `Inj1(e)` with `e` being the error that occurred while a successful computation would produce a `Inj2(r)` with `r` the result of the computation. We want to be able to discriminate these two cases, even when the types `Error` and `Result` are the same as it is usually the case when the computation need to compute a string or fail with an error message. Likewise, the injectivity of the constructors is mandatory to be able to the result produced `r` or the error that occurred `e`.

## Equivalence of inductive and functional forms

Once again any *coproductù can be *equivalently expressed* as a *data type* `Either_n_Ind[A_1, ..., A_n]` and as a *function type* `Either_n_Fun[A_1, ..., A_n]` with *inverse functions* `ind2fun` and `fun2ind` converting back and forth types `Either_n_Ind[A_1, ..., A_n]` and `Either_n_Fun[A_1, ..., A_n]`. For example with `n = 2`:

```scala
sealed abstract class Either2Ind[A,B]
final case class Inj1_Ind[A,B](value: A) extends Either2Ind[A,B]
final case class Inj2_Ind[A,B](value: B) extends Either2Ind[A,B]

trait Either2Fun[A,B] {
  def fold[R](inj1: A => R, inj2: B => R): R
}

def inj1_fun[A,B]: A => Either2Fun[A,B] =
  (a: A) =>
    new Either2Fun[A,B] {
      def fold[R](inj1: A => R, inj2: B => R): R = inj1(a)
    }

def inj2_fun[A,B]: B => Either2Fun[A,B] =
  (b: B) =>
    new Either2Fun[A,B] {
      def fold[R](inj1: A => R, inj2: B => R): R = inj2(b)
    }

def ind2fun[A,B]: Either2Ind[A,B] => Either2Fun[A,B] =
  (i: Either2Ind[A,B]) =>
    i match {
      case Inj1_Ind(a) => inj1_fun(a)
      case Inj2_Ind(b) => inj2_fun(b)
    }

def fun2ind[A,B]: Either2Fun[A,B] => Either2Ind[A,B] =
  (f: Either2Fun[A,B]) =>
    f.fold[Either2Ind[A,B]](Inj1_Ind[A,B] _, Inj2_Ind[A,B] _)
```

```haskell
data Either2Ind a b where
 Inj1_Ind :: a -> Either2Ind a b
 Inj2_Ind :: b -> Either2Ind a b

type Either2Fun a b = forall c. (a -> c) -> (b -> c) -> c

inj1_fun :: a -> Either2Fun a b
inj1_fun a f _ = f a

inj2_fun :: b -> Either2Fun a b
inj2_fun b _ g = g b

ind2fun :: Either2Ind a b -> Either2Fun a b
ind2fun (Inj1_Ind a) = inj1_fun a
ind2fun (Inj2_Ind a) = inj2_fun a

fun2ind :: Either2Fun a b -> Either2Ind a b
fun2ind f = f Inj1_Ind Inj2_Ind
```

## Generalization to a product of any number of types

The definition of `Either2` above can be adapted to `Either_n[A_1, ..., A_n]` for any value of `n` (i.e. *0*, *1*, *2*, *3*, *4*, ...). With `n = 0`, which means a coproduct of zero types, there is no constructor, which means it is impossible to build a value of this type. Thus `Either_0` is equivalent to `False` (i.e. `Either_0 ≅ False`). With `n = 1`, `Either_n[A] ≅ A`.

Further more for any types `A`, `B` and `C` we have the following properties

- `Either2[A,B]` is equivalent to `Either2[B,A]`
- `Either2[Either2[A,B], C]` and `Either2[A, Either2[B,C]]` and `Either3[A,B,C]` are all equivalent
- `Eithr2[False,A]` and  `Either2[A,False]` and `A` are all equivalent
- `Either_1[A]` is equivalent to `A`
- `(C, Either2[A,B])` is equivalent to `Either2[(C,A), (C,B)]`

*Exercise: write conversion functions `from` and `to`for each of these equivalence, such that `from` and `to` are inverse functions.*

Remember that in *Type Theory*, `False` is written `0`, `Unit` is written `1`, the *product* `(A_1, ..., A_n)` is written `A_1 × ... A_n` and the *coproduct* `Either_n[A_1, ..., A_n]` is written `A_1 + ... + A_n`. If we express the above properties using *Type Theory* notation, we get

- `A + B ≅ B + A`
- `(A + B) + C ≅ A + (B + C) ≅ A + B + C`
- `0 + A ≅ A + 0 ≅ A`
- `C × (A + B) ≅ (C × A) + (C × B)`
- `1 × A ≅ A`
- `0 × A ≅ A × 0 ≅ 0`

Is arithmetic familiar to you? If so such equations should be familiar too. But these times there are not between numbers but between equivalent types. If you ever wonder why [Algebraic Data Types](https://en.wikipedia.org/wiki/Algebraic_data_type) are called this way, this should give you some hints.

## [Next Episode: Recursive Data Types]({{< ref "types/5-rectypes.md" >}})

In the [next episode]({{< ref "types/5-rectypes.md" >}}), we will see **Recursive Data Types**.