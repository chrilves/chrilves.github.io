---
title: "Episode 5 - Recursive Data Types"
date: 2018-12-16T00:00:00+05:00
draft: false
type: "types"
episode: 5
episodeAsString: "fifth"
---

We have seen many types but we still don't know how to represent numbers, lists, trees, free monads, or any type with an infinite number of values. One again we will start by simple examples. Like always, **do no skim through** them but take the time to **develop a deep understanding**. If you feel uncomfortable with complex examples, it means you missed something important in the simple ones.

## A simple example

We will take as example an encoding of *non-negative* integers, also called **natural numbers**, i.e. numbers *0*, *1*, *2*, *3* and so on. There are actually many encodings possible, but we will take a simple encoding known as [Peano numbers](https://wiki.haskell.org/Peano_numbers). Did you ever wonder how *natural numbers* are built? There can actually be built starting from `0` then adding `1` to `0` to make `1`, then adding `1` to `1` to make `2`, then adding `1` to `2` to make `3`, then adding `1` to `3` to make `4`, and so on. Our encoding will mimic this construction. We need **two constructors**: one of represent `0` and the other to represent the operation of adding `1` to the previous number to make a new one.

Let's call `Nat` the type of natural numbers. The first constructor, representing `0` should be a constant of type `Nat` while the second one, representing the operation of adding `1` should be a function of type `Nat => Nat`. Remember that *constructors need to be injective*, but we are lucky, this operation is actually injective. Let call the first constructor `Zero:Nat` and the second one `Succ:Nat => Nat` (for successor). This is easy to translate into *Scala* and *Haskell*:

```scala
sealed abstract class Nat {
  def fold[A](zero: A, succ: A => A): A =
    this match {
      case Zero => zero
      case Succ(p) =>
        val a = p.fold(zero, succ)
        succ(a)
    }
}
final case object Zero         extends Nat
final case class  Succ(n: Nat) extends Nat
```

```haskell
data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat
```

Note that all the constructors we have seen in the previous episodes took as argument *already defined types*, but the first argument of `Succ` is of type `Nat`, the very *same type we are defining*. This is the reason why `Nat` is called a **recursive data type**: some of its constructors take as arguments values of type `Nat` itself. As usual:

- constructors are injective
- different constructors produces different value
- every value of type `Nat` is either a `Zero` or a `Succ(n)` for some `n:Nat`

The problem is, unlike *enumerations*, *products* and *coproducts* where these properties are enough to define their respective type (*up to equivalence*) without ambiguity. With *recursive data types* there can be several **non-equivalent** types for which these three properties hold. For example, the type `{0, 1, 2, ...}` of natural numbers and `{O, 1, 2,  ..., ∞}` in which we added one special number called infinity such that `Succ(∞) = ∞` both have the three properties above. So wee need to add a new *constraint*, which is that: **among all the types for which these properties hold, `Nat` is taken as the smallest one**.

*Recursive types* have amazing properties. For example the types `Nat` and `Option[Nat]` are equivalent! Indeed `nat2opt` and `opt2nat` are *inverse bijections*:

```scala
def opt2nat: Option[Nat] => Nat = {
  case Some(n) => Succ(n)
  case None => Zero
}

def nat2opt: Nat => Option[Nat] = {
  case Succ(n) => Some(n)
  case Zero => None
}
```

```haskell
opt2nat :: Maybe Nat -> Nat
opt2nat (Just n) = Succ n
opt2nat Nothing  = Zero

nat2opt :: Nat -> Maybe Nat
nat2opt (Succ n) = Just n
nat2opt Zero     = Nothing
```

It means that `Nat ≅ Option[Nat]`. Note that `Option[T] ≅ Either[Unit, T] ≅ 1 + T` in *Type Theory* notation, so `Nat` is actually one of the solution of the **type equation** `T ≅ 1 + T`. Such an equation means we are looking for types `T` such that `T` and `Option[T]` are equivalent. Regarding `Option` as a function from types to type, where the type `Option[T]` is the one obtained by applying the argument `T` to the function `Option`, `Nat` is one of the [fixed-point](https://en.wikipedia.org/wiki/Fixed_point_(mathematics)) of this function. More precisely, `Nat` is the least fixed-point of `Option`.

> Let `μ: (Type -> Type) -> Type` be the operator taking a type function `F` as argument (written `F[_]` in *Scala* and `f :: * -> *` in *haskell*) and returning the **least fixed-point** of `F`, which is defined as the smallest type `T` (up to equivalence) which is solution of the equation `T ≅ F[T]`. To simplify the notations, we may also write `μT.F[T]` instead of `μ(F)`.

As an example `Nat = μ(Option)` which we also write `Nat = μT.Either[Unit, T]` and also  `Nat = μT.(1 + T)`.

Another solution to the equation, which is, this time the *greatest fixed-point* of `Option` is the type `NatInf`, representing `{0, 1, 2, ..., ∞}`, defined as below. It is the *biggest type* which is solution (up to equivalence) of the equation `T ≅ 1 + T`. The two *inverse functions* `opt2natInf` and `natInf2opt` proves the equivalence:

```scala
trait NatInf {
  def unfold: Option[NatInf]
}

val zero: NatInf =
  new NatInf {
    def unfold: Option[NatInf] = None
  }

def succ(n: NatInf): NatInf =
  new NatInf {
    def unfold: Option[NatInf] = Some(n)
  }
  
val ∞ : NatInf =
  new NatInf {
    def unfold: Option[NatInf] = Some(∞)
  }

def opt2natInf: Option[NatInf] => NatInf = {
  case Some(n) => succ(n)
  case None    => zero
}

def natInf2opt: NatInf => Option[NatInf] =
  (n: NatInf) => n.unfold
```

```haskell
newtype NatInf = NatInf { unfold ::  Maybe NatInf }

zero :: NatInf
zero = NatInf Nothing

succ :: NatInf -> NatInf
succ n = NatInf (Just n)

inf :: NatInf
inf = NatInf (Just inf)

opt2natInf :: Maybe NatInf -> NatInf
opt2natInf (Just n) = succ n
opt2natInf Nothing  = zero

natInf2opt :: NatInf -> Maybe NatInf
natInf2opt = unfold
```

### Equivalence of inductive and functional definitions

`Nat` can be *equivalently defined* as a *data type* `NatInd` as well as a *function type* `NatFun`. Inverse bijections `ind2fun` and `fun2ind` prove `NatInf` and `NarInd` are equivalent:

```scala
sealed abstract class NatInd
final case object ZeroInd extends NatInd
final case class  SuccInd(n: NatInd) extends NatInd

trait NatFun {
  def fold[A](zero: A, succ: A => A): A
}

val zeroFun : NatFun =
  new NatFun {
    def fold[A](zero: A, succ: A => A): A = zero
  }

def succFun(n: NatFun) : NatFun =
  new NatFun {
    def fold[A](zero: A, succ: A => A): A = {
      val a = n.fold[A](zero, succ)
      succ(a)
    }
  }

def ind2fun: NatInd => NatFun =
  (i: NatInd) =>
    i match {
      case ZeroInd =>
        zeroFun
      case SuccInd(p) =>
        val n = ind2fun(p)
        succFun(n)
    }

def fun2ind: NatFun => NatInd =
  (n: NatFun) => n.fold[NatInd](ZeroInd, SuccInd(_))
```

```haskell
data NatInd where
  ZeroInd :: NatInd
  SuccInd :: NatInd -> NatInd

type NatFun = forall a. a -> (a -> a) -> a

zeroFun :: NatFun
zeroFun z _ = z

succFun :: NatFun -> NatFun
succFun n z s = s (n z s)

ind2fun :: NatInd -> NatFun
ind2fun  ZeroInd    = zeroFun
ind2fun (SuccInd n) = succFun (ind2fun n)

fun2ind :: NatFun -> NatInd
fun2ind n = n ZeroInd SuccInd
```

## Lists

Similarly, given a type `A`, we want to define the the type of lists whose elements are of type `A`, written `List[A]`. Let `l:List[A]` be a list whose elements are of type `A`. There are two cases: either the list is empty or it is not. Let's call the the empty list `Nil`. If the list is not empty, let `head` be its first element and `tail` the rest of the list (i.e. the same list as `l` but without the first element `head`). Then `tail` is also a list of type `List[A]` and `l` can be obtained by prepending `head` to `tail`. We will write this prepending operation `Cons :: (A, List[A]) => List[A]` such that `l = Cons(head, tail)`.

Once again we see we have **two constructors**: `Nil` of type `List[A]` and `Cons` of type `(A, List[A]) => List[A]`. Besides, these constructors satisfy the usual thee properties:

- constructors are injective
- different constructors produces different value
- every value of type `List[A]` is either a `Nil` or a `Cons(head, tail)` for some `head:A` and some `tail:List[A]`

Furthermore `List[A]` is the smallest type satisfying these properties. It can easily be defined in *Scala* as

```scala
sealed abstract class List[+A] {
  def fold[R](nil: R, cons: (A,R) => R): R =
    this match {
      case Nil => nil
      case Cons(head,tail) =>
        val r = tail.fold[R](nil, cons)
        cons(head, r)
    }
}
final case object Nil extends List[Nothing]
final case class  Cons[+A](head: A, tail: List[A]) extends List[A]
```

Like any recursive data type, `List[A]` is the smallest solution of a *type equation*. This time the equation is `T ≅ 1 + (A, T)` which in a more *Scalaish* syntax is `T ≅ Option[(A, T)]`. Equivalently, `List[A]` is also the *least fixed-point* of the type-function:

```scala
type F[T] = Option[(A, T)]
```

Which means `List[A] = μT.(1 + A × T)`. The biggest type which is solution (up to equivalence) of the equation, which is the *greatest fixed-point* of `F` is the type of streams whose elements are of type `A`, written `Stream[A]`:

```haskell
trait Steam[A] {
  def unfold: Option[(A, Stream[A])]
}
```

```haskell
newtype Stream a = Stream { runStream :: forall c. (Maybe (a, Stream a) -> c) -> c }
```

*Exercise: write the bijections proving `Stream[A] ≅ Option[(A, Stream[A])]`*

### Equivalence of inductive and functional definitions

`List[A]` can equivalently be defined as the *data type* `ListInd[A]` as well as the *type function* `ListFun[A]`. The two inverse functions `ind2fun` and `fun2ind` prove `ListInd[A]` and `ListFun[A]` are equivalent:

```scala
sealed abstract class ListInd[+A]
final case object NilInd extends ListInd[Nothing]
final case class  ConsInd[+A](head: A, tail: ListInd[A]) extends ListInd[A]

trait ListFun[+A] {
  def fold[R](nil: R, cons: (A,R) => R): R
}

def nilFun[A]: ListFun[A] =
  new ListFun[A] {
    def fold[R](nil: R, cons: (A,R) => R): R = nil
  }

def consFun[A](head: A, tail: ListFun[A]): ListFun[A] =
  new ListFun[A] {
    def fold[R](nil: R, cons: (A,R) => R): R = {
      val r = tail.fold[R](nil, cons)
      cons(head, r)
    }
  }

def ind2fun[A]: ListInd[A] => ListFun[A] =
  (i: ListInd[A]) =>
    i match {
      case NilInd =>
        nilFun[A]
      case ConsInd(head, tail) =>
        val tailFun = ind2fun(tail)
        consFun(head, tailFun)
    }

def fun2ind[A]: ListFun[A] => ListInd[A] =
  (f: ListFun[A]) => f.fold[ListInd[A]](NilInd, ConsInd(_,_))
```

```haskell
data ListInd a where
  NilInd  :: ListInd a
  ConsInd :: a -> ListInd a -> ListInd a

type ListFun a = forall r. r -> (a -> r -> r) -> r

nilFun :: ListFun a
nilFun nil _ = nil

consFun :: a -> ListFun a -> ListFun a
consFun head tail nil cons = cons head (tail nil cons)

ind2fun :: ListInd a -> ListFun a
ind2fun  NilInd             = nilFun
ind2fun (ConsInd head tail) = consFun head (ind2fun tail)

fun2ind :: ListFun a -> ListInd a
fun2ind f = f NilInd ConsInd
```

## Algebraic Data Types

[Algebraic Data Types](https://en.wikipedia.org/wiki/Algebraic_data_type) are types that can be expressed using only `False`, `Unit`, *products*, *coproducts* and the *least fixed-point* operator `μ`. For example, binary trees whose elements are of type `A`, defined in *Scala* by

```scala
sealed abstract class Tree[+A] {
  def fold[R](empty: R, leaf: A => R, node: (R, R) => R): R =
    this match {
      case Empty   => empty
      case Leaf(a) => leaf(a)
      case Node(l, r) =>
        val al = l.fold[R](empty, leaf, node)
        val ar = r.fold[R](empty, leaf, node)
        node(al, ar)
    }
}
final case object Empty extends Tree[Nothing]
final case class  Leaf[+A](value: A) extends Tree[A]
final case class  Node[+A](left: Tree[A], right: Tree[A]) extends Tree[A]
```

can be expressed as the type `Tree[A] = μT.(1 + A + (T × T))`, which is the smallest type (up to equivalence) solution of the equation `T ≅ Either3[Unit, A, (T, T)]`.

*Exercise: write the bijection proving the equivalence.*