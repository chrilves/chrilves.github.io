---
title: "Episode 3 - Products"
date: 2018-12-16T00:03:00+00:00
draft: false
type: "types"
episode: 3
episodeAsString: "third"
---

Products, often called *tuples*, *records* or **case classes**, are a convenient way to bundle values of different types into a single value. The **product** of `n` types (with `n` being *0*, *1*, *2*, etc) `A_1`, `A_2`, ..., `A_n` is precisely the type whose values are formed with exactly one value of each type `A_i` for `1 ≤ i ≤  n`. It is written `(A_1, A_2, ..., A_n)` in many languages, `Product_n[A_1, A_2, ..., A_n]` in *Scala* and `A_1 × ... × A_n` in *Type Theory*. As an example, the product of two types `A` and `B` would be defined as

```scala
final case class Product2[A, B](a: A, b: B) {
  def fold[R](f: (A, B) => R): R = f(a,b)
}
```

```haskell
data Product2 a b = Constructor a b
```

Although in practice this is the preferred way to define the product of two types `A` and `B`, we will in this presentation prefer the **equivalent** definition below:

```scala
sealed abstract class Product2[A,B] {
  def fold[C](f: (A, B) => C): C =
    this match {
      case Constructor(a,b) => f(a,b)
    }
}
final case class Constructor[A,B](a: A, b: B) extends Product2[A,B]
```

```haskell
data Product2 a b where
  Constructor :: a -> b -> Product2 a b
```

This *equivalent definition* makes clear the distinction between the type `Product2[A,B]` and the **constructor** `Constructor[A,B]`.  the constructor is a **function** taking as argument one value of type `A`, another value of type `B` and returning a value of type `Product2[A,B]`. Its type in *Haskell* is `A -> B -> Product2 A B` and in *Scala* `(A, B) => Product2[A,B]`. Note than in *Scala* functions of type `(A, B) => C` are functions of **two arguments** (precisely `Function2[A,B,C]`) and **not of one argument** `(A,B)` (which would be `Function1[(A,B), C]`):

```scala
scala> Constructor[Int, String] _ : ( (Int, String) => Product2[Int, String] )
res0: (Int, String) => Product2[Int,String] = $$Lambda$1620/2032136633@55be2608
```

```haskell
Prelude> :t Constructor
Constructor :: a -> b -> Product2 a b
```

Besides, by definition **constructors are injective functions**, which means that for any values `a1:A`, `a2:A`, `b1:B` and `b2:B`, `Constructor[A,B](a1,b1) == Constructor[A,B](a2,b2)` *if and only if* `a1 == a2` and `b1 == b2`
$$\forall a1,\ a2:\texttt{A},\ b1,\ b2:\texttt{B},\quad \texttt{Constructor[A,B]}(a1,b1) = \texttt{Constructor[A,B]}(a2,b2) \Longleftrightarrow a1 = a2 \textbf{ and } b1 = b2$$

Besides, `Constructor[A,B]` being the *only constructor* of `Product2[A,B]` then for any value `v` of type `Product2[A,B]`, there is a unique value `a` of type `A` and a unique value `b` of type `B` such that `v = Constructor[A,B](a,b)`
$$\forall v:\texttt{Product2[A,B]}\ \exists!\ (a:\texttt{A} \textrm{ and } b:\texttt{B}),\quad v = \texttt{Constructor[A,B]}(a,b)$$

## Equivalence of inductive and functional forms

Once again any product can be *equivalently expressed* as a *data type* `Product_n_Ind[A_1, ..., A_n]` and as a *function type* `Product_n_Fun[A_1, ..., A_n]` with *inverse functions* `ind2fun` and `fun2ind` converting back and forth types `Product_n_Ind[A_1, ..., A_n]` and `Product_n_Fun[A_1, ..., A_n]`. For example with `n = 2`:

```scala
sealed abstract class Product2Ind[A,B]
final case class ConstructorInd[A,B](a: A, b: B) extends Product2Ind[A,B]

trait Product2Fun[A,B] {
  def fold[C](f: (A, B) => C): C
}

def constructorFun[A,B](a: A, b: B): Product2Fun[A,B] =
  new Product2Fun[A,B] {
    def fold[C](f: (A, B) => C): C = f(a, b)
  }

def ind2fun[A,B]: Product2Ind[A,B] => Product2Fun[A,B] =
  (i: Product2Ind[A,B]) =>
    i match {
      case ConstructorInd(a,b) => constructorFun(a,b)
    }

def fun2ind[A,B]: Product2Fun[A,B] => Product2Ind[A,B] =
  (f: Product2Fun[A,B]) => f.fold[Product2Ind[A,B]](ConstructorInd[A,B] _)
```

```haskell
data Product2Ind a b where
  ConstructorInd :: a -> b -> Product2Ind a b

type Product2Fun a b = forall c. (a -> b -> c) -> c

constructorFun :: a -> b -> Product2Fun a b
constructorFun a b f = f a b

ind2fun :: Product2Ind a b -> Product2Fun a b
ind2fun (ConstructorInd a b) = constructorFun a b

fun2ind :: Product2Fun a b -> Product2Ind a b
fun2ind f = f ConstructorInd
```

## Generalization to a product of any number of types

The definition of `Product2` above can be adapted to `Product_n[A_1, ..., A_n]` for any value of `n` (i.e. *0*, *1*, *2*, *3*, *4*, ...). With `n = 0`, which means a product of zero types, the constructor takes zero argument, which means it is a constant. Thus `Product_0` is equivalent to `Unit` (i.e. `Product_0 ≅ Unit`). This is the reason why `Unit` is sometimes written `()`. With `n = 1`, `Product_n[A] ≅ A`.

Further more for any types `A`, `B` and `C` we have the following properties

- `(A , B)` is equivalent to `(B , A)`
- `((A, B), C)` and `(A, (B, C))` and `(A, B, C)` are all equivalent
- `(Unit, A)` and  `(A, Unit)` and `A` are all equivalent
- `(A)` is equivalent to `A`
- `(False, A)` and `(A, False)` and `False` are all equivalent

*Exercise: write conversion functions `from` and `to`for each of these equivalence, such that `from` and `to` are inverse functions.*

## [Next Episode: CoProducts]({{< ref "types/4-coproducts.md" >}})

In the [next episode]({{< ref "types/4-coproducts.md" >}}), we will a convenient way to express alternatives: **CoProducts**.