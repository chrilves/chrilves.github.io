---
title: "Let's meet the charming `fold` family"
date: 2018-11-30T14:13:12+01:00
draft: false
---

Today we will meet an amazing family: the `fold` functions!

# The well known `foldRight`

[Lists](https://en.wikipedia.org/wiki/List_(abstract_data_type)) is one of the first
data structure every developer/computer scientist meet in her/his journey into programming:

```scala
sealed abstract class List[+A]
final case object Nil                              extends List[Nothing]
final case class  Cons[+A](head: A, tail: List[A]) extends List[A]
```

It means means values of type `List[A]` can be of (only) two forms:

- either `Nil`
- or `Cons(head, tail)` for some values `head` of type `A` and `tail` of type `List[A]`

For example we can define the following lists:

```scala
val empty : List[Int] = Nil
val l1 : List[Int] = Cons(61, Nil)
val l2 : List[Int] = Cons(34, Cons(61, Nil))
val l3 : List[String] = Cons("a", Cons("b", Cons("c", Nil)))
```

In addition, `Nil` and `Cons` can be seen as constants and functions returning `List[A]`:

```scala
def nil[A]: List[A] = Nil
def cons[A](head: A, tail: List[A]): Lis[A] = Cons(head, tail)
```

The **fold** function, often called `foldRight`, answers the question:

> *What would have happened if, instead of having used `Nil` and `Cons` in the construction of a list `l:List[A]`, we would have used
another constant `z:T` and another function `f:(A, T) => T` for some type `T`?*

Let's illustrate this using the previous examples:

```scala
val empty : Int = 0 // z = 0
val v1 : Int = max(61, 0) // z = 0, f = max
val v2 : Int = mult(34, mult(61, 1)) // z = 1, f = mult
val v3 : String = concat("a", concat("b", concat("c", "")))
  -- z = "", f = concat
```

The definition of `foldRight` illustrates well the transformation process. It deconstructs the list `l:List[A]`
and replace `Nil` by `z` and `Cons` by `f`:

```scala
def foldList[A,T](z: T, f: (A,T) => T): List[A] => T = {
  def transform(l: List[A]): T =
    l match {
      case Nil => z
      case Cons(head, tail) =>
        val transformedTail = transform(tail)
        f(head, transformedTail)
    }
  
  transform _
}
```

# The simple cases: Enum Types

`fold` functions can be defined for a wide range of data structures. As a first example, let's take this type:

```scala
sealed abstract class SingletonType
final case object SingleValue extends SingletonType
```

The type `SingletonType` admits one and only one value: `SingleValue`. Folding over `SingletonType` means,
replacing `SingleValue` by a constant `z:T` for some type `T` :

```scala
def foldSingletonType[T](z:T): SingletonType => T = {
  def transform(v: SingletonType): T =
    v match {
      case SingleValue => z
    }

  transform _
}
```

While `SingletonType` has only one value, the type `Boolean` have exactly two values `True` and `False`:

```scala
sealed abstract class Boolean
final case object True  extends Boolean
final case object False extends Boolean
```

So folding over `Boolean`s mean, given a type `T` and two constants `tt:T` and `ff:T`, replacing `True` by `tt` and `False` by `ff`:

```scala
def foldBoolean[T](tt:T, ff:T): Boolean => T = {
  def transform(v: Boolean): T =
    v match {
      case True  => tt
      case False => ff
    }

  transform _
}
```

And so on for every **enum type**.

# Beyond enums

You may start the see general process. If values of type `C` are build using **constructors** (`Nil` and `Cons[A]` for `List[A]`,
`SingleValue` for `SingletonType`, `True` and `False` for `Boolean`), then **folding is all about transforming values of type `C`
into another type `T` by replacing each constructor of `C` by a constant or function on `T` of the same shape**. Let's consider
the type `Either[A,B]`:

```scala
sealed abstract class Either[A,B]
final case class Left[A,B](value: A)  extends Either[A,B]
final case class Right[A,B](value: B) extends Either[A,B]
```

To transform values of type `Either[A,B]` into `T` we need two functions on `T`:

- `Left` being of type `A => Either[A,B]` we need a function `f: A => T`.
- `Right` being of type `B => Either[A,B]` we need a function `g: B => T`.

Then we can operate the transformation:

```scala
def foldEither[A,B,T](f: A => T, g: B => T): Either[A,B] => T = {
  def transform(v: Either[A,B]): T =
    v match {
      case Left(a)  => f(a)
      case Right(b) => g(b)
    }

  transform _
}
```

# Recursive Types

Folding over recursive types obey the previous rules. Recursion is handled by transforming sub-terms first. Let's consider
the type of binary trees:

```scala
sealed abstract class Tree[+A]
final case object Empty extends Tree[Nothing]
final case class  Node[+A](value:A, left: Tree[A], right: Tree[A]) extends Tree[A]
```

To transform values of type `Tree[A]` into `T` we need:

- `Empty` being a constant of type `Tree[A]`, we need a constant `z:T`.
- `Node` being a function of type `(A, Tree[A], Tree[A]) => Tree[A]` we need a function `f: (A, T, T) => T`.
  Note how all occurrences of `Tree[A]` have been replaced by `T` in the type.

Then we can operate the transformation:

```scala
def foldTree[A,T](z: T, f: (A, T, T) => T): Tree[A] => T = {
  def transform(v: Tree[A]): T =
    v match {
      case Empty => z
      case Node(a,l,r) =>
        val g: T = transform(l) // Transforming sub-term l
        val d: T = transform(r) // Transforming sub-term r
        f(a,g,d)
    }

  transform _
}
```

# Generalized Algebraic Data Types (GADT)

Instead of giving a formal definition of what [Generalized Algebraic Data Types](https://en.wikipedia.org/wiki/Generalized_algebraic_data_type)
i will show you some examples.

## Type Equalities

Consider the type:

```scala
sealed abstract class EmptyOrSingleton[A]
final case object SingleValueIfAisInt extends EmptyOrSingleton[Int]
```

This type looks very similar to `SingletonType` but, while `SingleValue` was **always** a value of `SingletonType`,
`SingleValueIfAisInt` is **only** a value of `EmptyOrSingleton[Int]`, i.e. when `A` is `Int`. So what happens to
`EmptyOrSingleton[A]` when `A` is not `Int`? Then there is no constructor for `EmptyOrSingleton[A]` so no value
for `SingletonIfInt[A]` (excluding `null` which we will pretend no to exist).

GADTs are very useful to encode predicates over types. Imagine you have a **value** `v:EmptyOrSingleton[A]` for
some type `A`  (remember we pretend `null` does not exist). What could you say about `A`? The **only** way to get
a **value** of type `EmptyOrSingleton[A]` is through `SingleValueIfAisInt`. Thus `v` is `SingleValueIfAisInt`
which is of type `EmptyOrSingleton[Int]` so is `v`. We can conclude that `A` is actually `Int`. Not convinced?
Let `A` be `String`, can you build a **value** of type `EmptyOrSingleton[String]` without using `null`? Try it.

To find how to fold `EmptyOrSingleton[A]` into `T`, let's apply the technique we used in the previous sections.
`EmptyOrSingleton[A]` has only one constructor, `SingleValueIfAisInt`,  so we need a constant `z:T`. But
`SingleValueIfAisInt` is not of type `EmptyOrSingleton[A]` but `EmptyOrSingleton[Int]`. The argument `A` matters
so let `T` depend on `A`: we want to transform values of type `EmptyOrSingleton[A]` into `T[A]`.

- `SingleValueIfAisInt` being of type `EmptyOrSingleton[Int]` we need a constant `z:T[Int]`

Then we can operate the transformation:

```scala
def foldEmptyOrSingleton[A, T[_]](z: T[Int]): EmptyOrSingleton[A] => T[A] = {
  def transform(v: EmptyOrSingleton[A]): T[A] =
    v match {
      case SingleValueIfAisInt => z // Because we know A = Int
    }

  transform _
}
```

`foldEmptyOrSingleton` means that, for some `T[_]`, if you have a value `z:T[Int]` then you can transform any value
`EmptyOrSingleton[A]` into `T[A]`. For example, let's take

```scala
type T[X] = X =:= Int
val z:T[Int] = implicitly[Int =:= Int]
```

Then `foldEmptyOrSingleton[A,T](z)` gives us, for any value `v:EmptyOrSingleton[A]` a proof that `A =:= Int`.

## Existential Quantification

GADTs not only provide useful type equalities, they also offer existential quantification!

```scala
sealed abstract class Ex[F[_]] {
  type hidden
  val value: hidden
  val evidence: F[hidden]
}
final case class MakeEx[F[_],A](value: A, evidence: F[A]) extends Ex[F] {
  type hidden = A
}
```

Any value `v:Eq[F]` has to be an instance of `MakeEx[F,A]` for some type `A`. Which means we have a value,
`v.value`, of type `A` and an instance of the type-class `F` for `A` (for example an instance of `Monoid[A]`
with `F[X] = Monoid[X]`).

To transform values of type `Ex[F]` into `T` we need:

- `MakeEx[F[_],?]` being of type `[A](A, F[A]) => Ex[F]` meaning: `For_all_type A, (A, F[A]) => Ex[F]`, we
  need a function `f` of type `[A](A, F[A]) => T`. Because *Scala* does not support transparent higher ranked
  types, we need to emulate them with a `trait`:

```scala
trait Elim[F[_],T] {
  def apply[A](value: A, evidence: F[A]): T
}
```

Then we can operate the transformation:

```scala
def foldEx[F[_], T](f: Elim[F, T]): Ex[F] => T = {
  def transform(v: Ex[F]): T =
    v match {
      case w@MakeEx(value, evidence) => f[w.hidden](value, evidence)
    }

  transform _
}
```

# Duality

In this post we have deduced the `fold` functions from the definition of each type. It is possible to
do the opposite: each constructor can be derived from the `fold` function of its type. For example:

```scala
trait List[+A] {
  def fold[T](z:T, f: (A,T) => T): T
}

def nil[A]: List[A] =
  new List[A] {
    def fold[T](z:T, f: (A,T) => T): T = z
  }

def cons[A](head:A, tail: List[A]): List[A] =
  new List[A] {
   def fold[T](z:T, f: (A,T) => T): T = f(head, tail.fold(z,f))
  }

def equality[A](l1: List[A], l2:List[A]): Boolean = ??? // Difficult but worthy exercice
```

# Conclusion

I hope i convinced you folds are immensely useful. First, they let us write simply complex transform functions.
But this not the most interesting property. It is sometimes easier to define a type by its `fold` function.
*Java*, for example, does not have support for neither `sealed` classes nor pattern-matching. How could we define
the `List` type so that `Nil` and `Cons` are the two only cases? The `fold` function forces any instance of `List`
to fit into the desired shape (if some rules are obeyed like no `null` and no runtime-reflection).
It can also happen that type-inference is not smart enough, `fold` function provide an alternative way which is
often easier for the `Scala` type-checker.
