---
title: "Episode 2 - Enumerations"
date: 2018-12-16T00:00:00+02:00
draft: false
type: "types"
episode: 2
episodeAsString: "second"
---

Now that we know what types are and why there are useful, it is about time to meet some remarkable ones. But before we start, there is some important things to state. As a developer i know how tempting it is to search ready-to-paste answers. But *the subject of these post series is nothing like a ready-to-paste answers cookbook*. On the contrary, this is a presentation of **deep, difficult but rewarding concepts**. Do not see them as patterns to follow blindly, like it is unfortunately too often the case in our field. Instead, **take the time to develop a deep understanding** of what is really going on.

As such, the example i give may look simple. They are indeed much simpler than in real-life applications. They are not meant to be applicable as-is but to shed light on the core ideas. If you understand the principles they rely on, you'll be able to apply these powerful concepts in your day to day code. **Do not skim through** this but take the time to develop your understanding.

In this whole series is we will assume `null` (in *Scala*), `undefined` (in *Haskell*) and [side-effects](https://en.wikipedia.org/wiki/Side_effect_(computer_science)) do not exist. `null`, `undefined` and *side-effects* are *absurd* in every possible way. They make interesting properties collapse and should be *avoided at all costs*! We will consider all functions from `A` to `B` to be **mathematical functions**:

- they always return a valid value of type `B` of any input input value of type `A`
- on the same argument (or an equal argument), they always return the same result (or an equal result)
- the only effect of executing them is obtaining a result, no side-effect allowed
- If `b:B = f(a:A)` we can always replace `b` by `f(a)` and vice-versa, anywhere in the program, without altering its behavior (apart from computation time and memory consumption)

Formally it means we will consider all functions [referentially transparent](https://en.wikipedia.org/wiki/Referential_transparency) and [total](https://en.wikipedia.org/wiki/Total_functional_programming). That being said, let the journey begin with a very useful concept.

## Many equivalent ways to express it

There is often many ways to express the same idea. How would you represent a piece of text? The types `List[Char]` and `String` are two different ways to do so. These types are indeed not equal: they are not encoded the same way and have distinct performance properties. But despite their differences, they can be considered equivalent. There is indeed  a one-to-one correspondence between them: we can define two functions `list2string: List[Char] => String` and `string2list: String => List[Char]`:

```scala
def list2string: List[Char] => String =
  (l:List[Char]) => l.foldLeft("") {
    case (str,char) => s"$str$char"
  }

def string2list: String => List[Char] =
  (str: String) => str.toList
```

such that `list2string` and `string2list` are the **inverse of each other**
$$\forall s:\texttt{String},\quad \texttt{string2list}(\texttt{list2string}(s)) = s$$
$$\forall l:\texttt{List[Char]},\quad \texttt{list2string}(\texttt{string2list}(l)) = l$$

which also means

- for any list there is a unique string $$\forall l:\texttt{List[Char]}\ \exists! s:\texttt{String}  ,\quad \texttt{string2list}(s) = l$$
 
- for any string there is a unique list $$\forall s:\texttt{String}\ \exists! l:\texttt{List[Char]},\quad \texttt{string2list}(\texttt{list2string}(s)) = s$$

- `list2string` and `string2list` are **injective**
  $$\forall l1, l2:\texttt{List[Char]},\ \texttt{list2string}(l1) = \texttt{list2string}(l2) \implies l1 = l2$$
  $$\forall s1, s2:\texttt{String},\ \texttt{string2list}(s1) = \texttt{string2list}(s2) \implies s1 = s2$$

Basically it means we can always convert from one type to the other without loss of generality.

> For any function `f: A => B` (i.e. from `A` to `B`), `f` is called a **bjection** *if and only if* there exists a function `g: B => A` (i.e. from `B` to `A`) such that `f` and `g` are *inverse of each other*:
$$\forall a:\texttt{A},\quad \texttt{g}(\texttt{f}(a)) = a$$
$$\forall b:\texttt{B},\quad \texttt{f}(\texttt{g}(b)) = b$$
Two types `A` and `B` are called **equivalent**, which is written `A ≅ B`, *if and only if* there exists a *bijection* `f: A => B`.

This notion is *fundamental* as it tells us that similarity between types is how closely they behave, not how closely they look. Two types of apparent very different form may actually be equivalent while two whose form may seem close may be fundamentally at odds. Focus on properties, not on looks.

## The empty type

The first type i want to present has many names. [Scala](https://www.scala-lang.org) names it `Nothing`, [Elm](https://elm-lang.org) calls it `Never`, [Coq](https://coq.inria.fr) uses the name it usually has in [Proof Theory](https://en.wikipedia.org/wiki/Proof_theory) which is `False` while [Type Theory](https://en.wikipedia.org/wiki/Type_theory) refers to it as `0`. This is an **uninhabited type** which means there is no value of this type. You may look at it as the empty set (hence the name `0`):

```scala
final abstract class False {
  def fold[A]: A
}
```

This class being `final` we cannot define sub-classes, but being also `abstract` we cannot instantiate it either! And even if we could, how would we write a function `fold` which meets the specification that it provides a value of type `A` for any `A`? Remember, `null` does not exists.

How a type without values can be useful? By representing impossible cases in the type-system. Let `v` be a value of type `Either[False, Int]`. `v` has to be `Right(i)` for some integer `i` because otherwise it would be `Left(f)` for some value `f` of type `False` which is impossible! It means the following pattern-matching is exhaustive.

```scala
def extractRight[A](v: Either[False,A]): A =
  v match {
    case Right(a) => a
  }
```

Unfortunately *Scala* protests it's not, demanding we cover the case `v = Left(f: False)` by returning a value of type `A`. We know, assuming `null` does not exists, there can not be such a value `f:False`. How to make *Scala* happy? Fortunately, every hypothetical value of type `False` comes with a handy `fold` function able to return anything. It corresponds in logic to [the principle of explosion](https://en.wikipedia.org/wiki/Principle_of_explosion) stating that from a contradicton (having a value of an empty type for example), you can conclude anything:

```scala
def extractRight[A](v: Either[False,A]): A =
  v match {
    case Right(a) => a
    case Left(f) => f.fold[A]
  }
```

Note that `extractRight` and `Right:A => Either[False, A]` being *inverse functions* it means `Either[False, A]` is equivalent to `A`. This one is of the main properties of `False`: `Either[False, A]`, `Either[A, False]` and `A` are all equivalent (i.e. `Either[False, A] ≅ A ≅ Either[A, False]`).

You could argue using `Either[False, A]` is pointless as it is equivalent to `A` which is true, but bare in mind it is just a toy example. A more realistic one would involve complex types such as bifunctors like `IO[E,A]`. The ability to express impossible cases let us write *IO* computations that never fails `IO[False, A]` and *IO* computation that never returns `IO[E,False]` with the same code and [API](https://en.wikipedia.org/wiki/Application_programming_interface) instead of relying on code/API duplication which is a *waste of time, effort and complexity*.

The idea being `False` can be *expressed in many equivalent ways*. The three types `False`, `FalseInd` and `FalseFun` are equivalent (``False ≅ FalseInd ≅ FalseFun`):

```scala
final abstract class FalseInd

trait FalseFun {
  def fold[A]: A
}
```

This is even clearer in [Haskell](https://www.haskell.org/ghc) as shown below where `ind2fun` and `fun2ind` are *inverse functions*.

```haskell
{-# LANGUAGE RankNTypes, EmptyCase #-}
module Iso where

data FalseInd
type FalseFun = forall a. a

ind2fun :: FalseInd -> FalseFun
ind2fun x = case x of { }

fun2ind :: FalseFun -> FalseInd
fun2ind x = x
```

In *Scala* like in *Haskell* `FalseInd` express the idea of a **data type with no constructor** (i.e. to way to build a value) while `FalseFun` is the **type of a function** with no argument (i.e. a constant) returning values of type `A` for any `A`, but such a function does not exists. It may seem weird having functions between empty types but it actually makes a lot of sense: there is indeed a one-to-one correspondence between these two types as both have no values at all.

## The singleton type

Often called `unit`, `()` or `1`, it is the type with **only one value**.

```scala
sealed abstract class Unit {
  def fold[A](x: A): A =
    this match {
      case Unique => x
    }
}
final case object Unique extends Unit
```

Note that the type `Unit` admits *one and exactly one value* which is `UniqueInd`. `UniqueInd` is called a **constructor of the data type `Unit`** as it one way (the only one actually) to build a value of type `Unit`. `Unit` is useful everywhere you have to fill a value and/or a type but you actually don't care which. For example, let `v = println("Hello World"):IO[Unit]` be the value printing a message to the screen. We don't care what the execution `v` returns, we are only interested into what is printed, but it still needs to return something for the program to continue.

Once again `Unit` can be *equivalently expressed* as both the data type `UnitInd` and the *function type* `UnitFun`. Indeed `ind2fun` and `fun2ind` are *inverse functions* showing `Unit ≅ UnitInd ≅ UnitFun`.

```scala
sealed abstract class UnitInd
final case object UniqueInd extends UnitInd

trait UnitFun {
  def fold[A](x: A): A
}

val uniqueFun: UnitFun =
  new UnitFun {
    def fold[A](x: A): A = x
  }

def ind2fun: UnitInd => UnitFun =
  (i: UnitInd) => i match {
    case UniqueInd => uniqueFun
  }

def fun2ind: UnitFun => UnitInd =
  (f: UnitFun) => f.fold[UnitInd](UniqueInd)
```

```haskell
data UnitInd where
  UniqueInd :: UnitInd

type UnitFun = forall a. a -> a

uniqueFun :: UnitFun
uniqueFun x = x

ind2fun :: UnitInd -> UnitFun
ind2fun UniqueInd = uniqueFun

fun2ind :: UnitFun -> UnitInd
fun2ind u = u UniqueInd
```

The type `UnitFun` may surprise you. How is it possible that a function type like it has only one possible value? **Try** to write a *referentially transparent* and *total* instance of `UnitFun`. You will see there is only one way to do so.

One of the main properties of `Unit` is that `(Unit, A)`, `(A, Unit)` and `A` are equivalent (i.e. `(Unit, A) ≅ A ≅ (A, Unit)`). *Exercise: write the inverse functions between these types.*

## The two values type: Booleans

Here comes one of the most well known type: the one with **exactly two values** often called `Boolean` in programming and `2` in *Type Theory*. Its values are generally named `true` and `false`:

```scala
sealed abstract class Boolean {
  def fold[A](tt: A, ff: A): A =
    this match {
      case True  => tt
      case False => ff
    }
}
final case object True  extends Boolean
final case object False extends Boolean
```

I'm sure you're already familiar with booleans, but did you recognize the `fold` function? It is generally named `if-then-else`:

```scala
def ifThenElse[A](b: Boolean)(tt: A, ff: A): A = b.fold[A](tt, ff)
```

`True` and `False` are called the **constructors of `Boolean`** because they are the **only way to build values** of type `Boolean`. Once again this idea of a type with two values is *equivalently expressed* both as the *data type* `BoolInd` and as the *function type* `BoolFun` . Functions `ind2fun` and `fun2ind` are indeed *inverse* of each other.

```scala
sealed abstract class BoolInd
final case object TrueInd  extends BoolInd
final case object FalseInd extends BoolInd

trait BoolFun {
  def fold[A](tt: A, ff: A): A
}

val trueFun: BoolFun =
  new BoolFun {
    def fold[A](tt: A, ff: A): A = tt
  }

val falseFun: BoolFun =
  new BoolFun {
    def fold[A](tt: A, ff: A): A = ff
  }

def ind2fun: BoolInd => BoolFun =
  (i: BoolInd) => i match {
    case TrueInd  => trueFun
    case FalseInd => falseFun
  }

def fun2ind: BoolFun => BoolInd =
  (f: BoolFun) => f.fold[BoolInd](TrueInd, FalseInd)
```

```haskell
data BoolInd where
  TrueInd  :: BoolInd
  FalseInd :: BoolInd

type BoolFun = forall a. a ->  a -> a

trueFun :: BoolFun
trueFun tt _ = tt

falseFun :: BoolFun
falseFun _ ff = ff

ind2fun :: BoolInd -> BoolFun
ind2fun TrueInd  = trueFun
ind2fun FalseInd = falseFun

fun2ind :: BoolFun -> BoolInd
fun2ind u = u TrueInd FalseInd
```

Once again the type `BoolFun` may surprise you. Once again **try** to write a *referentially transparent* and *total* instance of `BoolFun`. You will see there are only two ways to do so.

I could continue forever presenting types with *3*, *4*, ... values but by now you must see the same pattern repeating and repeating again.

## [Next Episode: Products]({{< ref "types/3-products.md" >}})

In the [next episode]({{< ref "types/3-products.md" >}}), we will a convenient way to combine types: **Products**.