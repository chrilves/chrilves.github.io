---
title: "Recursion Schemes: the high-school introduction"
date: 2019-03-22T10:00:00+01:00
draft: false
description: Presentation of recursion schemes from simple examples without the complex vocabulary in the way.
keywords:
  - Recursion Schemes
  - Algebra
  - F-Algebra
---

*I'll be giving a talk on Thursday the 28th of march 2019 at the [96th Paris Scala User Group](https://www.meetup.com/fr-FR/Paris-Scala-User-Group-PSUG/events/259498147/) session on about this.*

Recursion schemes are said to be a tough subject. Articles and presentations often
flood the audience with lots of names such as *Algebra*, *CoAlgebra*, *catamorphisms*,
*anamorhpisms*, *hylomorphism*, etc. Is knowing all these concepts required to understand
recursion schemes? I have good news for you: it isn't! All you need, to see what recursion
schemes are and why there are useful, can be presented with just a single basic function,
often taught as an introduction to programming: factorial. I'm glad to welcome you to
to the high-school introduction to recursion scheme 😉.

## Learning Recursion Schemes

Before diving into the subject, let's take a moment to contextualize.
Recursion-schemes, like most of advanced functional programming techniques,
is almost never taught in programming courses or books. It means there is a strong chance
the subject, and the concepts it relies upon, is totally new to you.
I want you to remember you haven't learnt programming in one day, and you probably did not
start learning programming by implementing a distributed steaming application over
a spark cluster from scratch.
Like most of us, you probably started by coding some sort of *Hello, World!*.
Let's face it, real business application are a lot more complex than this.
Do you imagine what a first introduction to programming would be, if instead of asking
people to write a simple *Hello, World!*, we would ask them to write a real state-of-the-art
large-scale business application that meets all the requirements we expect in
production nowadays? **Learning takes time! Start with toy examples that are
indeed far from real-word cases but enables you to grow your understanding, one step at a time.**

The examples below are indeed toy examples. When i develop with recursion scheme, like any
specialist in any field, i use specialist techniques and vocabulary (you know, the usual vocabulary
from category and type theory). But if you're reading this, it probably means you're not a
recursion-scheme specialist yet. Using complex words flatters our ego, which
is very enjoyable, but developing a deep understanding of these notions is far better! So let's put our ego
aside for a moment and accept to start with the basics.

## In *Recursion Schemes*, there is *Recursion*

First of all, let me present you the famous *factorial* function. It is defined on *non-negative* numbers *n* as the
product of all numbers between *1* and *n* included: 

$$fact(n) = 1 \times 2 \times 3 \times \cdots \times n$$

To ease the presentation we will take `Int` as the type of *non-negative integers*.
Obviously in production code negative values should be handled appropriately
but for simplicity's sake, we will define `fact` in *Scala* and in *Haskell* as

```scala
def fact(n: Int): Int =
  if (n == 0) 1
  else {
    val r = fact(n-1)
    n * r
  }
```

```haskell
fact :: Int -> Int
fact 0 = 1
fact n = let r = fact (n - 1)
         in n * r
```

Factorial is written here as a recursive function.
As you probably know, it can also be written as an
iterative one (using a `for` or `while` loop) but the subject of this article is *Recursion
Schemes*, not *Iterative Schemes*, so let's use recursion. This function computes `fact(2)` as follows:

- `fact(2) = 2 * fact(1)` so it needs to compute `fact(1)`
- `fact(1) = 1 * fact(0)` so it needs to compute `fact(0)`
- `fact(0) = 1`
- now that the result of `fact(0)` is known, it can replace the call of `fact(0)`
  by its result which gives `fact(1) = 1 * fact(0) = 1 * 1 = 1`
- now that the result of `fact(1)` is known, it can replace the call of `fact(1)`
  by its result which gives `fact(2) = 2 * fact(1) = 2 * 1 = 2`.

Look at how `fact(n)` is calling it-self: if `n = 0` then it doesn't call itself, otherwise
it calls itself on `n - 1`. Let's split this definition in two parts: the first one contains all the code
relevant to how `fact` is calling itself but only it, the second one is made of the rest. There is no clear
rule for what is relevant and what is not. Different splits may work, they will just give rise to different
schemes, which is not a problem at all. You just need to find one that fits your needs.

For `fact`, the key element to note is it is not calling itself when `n = 0` but otherwise calls itself
with `n - 1`. The constant returned in the `n = 0` case and the operation done in the other one have no impact
on how `fact` recurses. So i choose to split it by taking all code not relevant to recursion out of
its body:

```scala
/* Part irrelevant to recursion:
 * The real definitions of these variables
 * have no impact on how fact is calling itself
 */
val baseCase: Int = 1
def recCase(n: Int, r: Int): Int = n * r

/* Recursion-only part:
 * The only implementation details it contains
 * are about how fact it calling itself
 */
def fact(n: Int): Int =
  if (n == 0) baseCase
  else {
    val r = fact(n-1)
    recCase(n, r)
  }
```

```haskell
{-
 Part irrelevant to recursion:
 The real definitions of these variables
 Have no impact on how fact is calling itself
-}
baseCase :: Int
baseCase  = 1

recCase :: Int -> Int -> Int
recCase n r = n * r
 
{-
 Recursion-only part:
 The only implementation details it contains
 are about how fact it calling itself
 -}
fact :: Int -> Int
fact 0 = baseCase
fact n = let r = fact (n-1)
         in recCase n r
```

Let me present you another function, also defined on *non-negative* numbers *n*,
but that computes this time the *sum*  of all numbers between *1* and *n* included:

$$sum(n)  = 1 + 2 + 3 + \cdots + n$$

```scala
def sum(n: Int): Int =
  if (n == 0) 0
  else {
    val r = sum(n-1)
    n + r
  }
```

```haskell
sum :: Int -> Int
sum 0 = 0
sum n = let r = sum (n - 1)
        in n + r
```


We can apply the same technique to `sum`: splitting the definition into two parts,
one containing all but only recursion-relevant code, and the other the rest. It gives:

```scala
/* Part irrelevant to recursion:
 * The real definitions of these variables
 * have no impact on how sum is recurs
 */
val baseCase: Int = 0
def recCase(n: Int, r: Int): Int = n + r

/* Recursion-only part:
 * The only implementation details it contains
 * are about how fact it recurs
 */
def sum(n: Int): Int =
  if (n == 0) baseCase
  else {
    val r = sum(n-1)
    recCase(n, r)
  }
```

```haskell
{-
 Part irrelevant to recursion:
 The real definitions of these variables
 Have no impact on how fact is calling itself
-}
baseCase :: Int
baseCase = 0

recCase :: Int -> Int -> Int
recCase n r = n + r
 
{-
 Recursion-only part:
 The only implementation details it contains
 are about how fact it calling itself
 -}
sum :: Int -> Int
sum 0 = baseCase
sum n = let r = sum (n-1)
        in recCase n r
```

Do you see how similar the recursion-relevant parts of `sum` and  `fact` are? They are actually identical! It means
`fact` and `sum` have the same recursion structure. The recursion-irrelevant part differ:
the constant `baseCase` which is *1* in `fact` but *0* in `sum` and operation `recCase` which is `n * r` in `fact` but
`n + r` in `sum`. Note that if we replace, in each case, occurrences of `baseRec` and `recCase` by their definition, we
get back the original functions. Look at the common recursive-relevant part:

```scala
def commonRecursiveRelevantPart(n: Int): Int =
  if (n == 0) baseCase
  else {
    val r = commonRecursiveRelevantPart(n-1)
    recCase(n, r)
  }
```

```haskell
commonRecursiveRelevantPart :: Int -> Int
commonRecursiveRelevantPart 0 = baseCase
commonRecursiveRelevantPart n = let r = commonRecursiveRelevantPart (n-1)
                                in recCase n r
```

Obviously, for this code to be correct, `baseCase` and `recCase` have to be defined. Let's fix this by taking them
as arguments:

```scala
def scheme(baseCase: Int, recCase: (Int, Int) => Int): Int => Int = {
  def commonRecursiveRelevantPart(n: Int): Int =
    if (n == 0) baseCase
    else {
      val r = commonRecursiveRelevantPart(n-1)
      recCase(n, r)
    }
  
  commonRecursiveRelevantPart
}
```

```haskell
scheme :: Int -> (Int -> Int -> Int) -> Int -> Int
scheme baseCase recCase = commonRecursiveRelevantPart
  where
    commonRecursiveRelevantPart :: Int -> Int
    commonRecursiveRelevantPart 0 = baseCase
    commonRecursiveRelevantPart n = let r = commonRecursiveRelevantPart (n-1)
                                    in recCase n r
```

It is then trivial to define both `fact` and `sum` by feeding `scheme` with corresponding definitions for `baseCase` and `recCase`:

```scala
def fact: Int => Int = scheme(1, (n: Int, r:Int) => n * r)
def sum : Int => Int = scheme(0, (n: Int, r:Int) => n + r)
```

```haskell
fact :: Int -> Int
fact = scheme 1 (*)

sum :: Int -> Int
sum = scheme 0 (+)
```

We can now give a first answer to how recursion schemes can be useful. They enable to
to write less code which is both easier and safer. But there is more! Recursive calls, like any
function calls, consume the stack. If there are too many recursive calls (i.e. when `n` is to big),
there is a risk of stack overflow. Some languages like *Scala* are smart enough to avoid, in some cases,
this problem by transforming *tail-recursive* functions into iterative loops.
Unfortunately not all recursive functions are *tail-recursive*. Writing recursive functions as
iterative loops is not the solution either since it is intricate and error-prone.
Fortunately it is enough to only write the recursion scheme once:

```scala
def scheme(baseCase: Int, recCase: (Int, Int) => Int)(n: Int): Int = {
  var res = baseCase
  var i: Int = 1
  while (i <= n) {
    res = recCase(i, res)
    i += 1
  }
  res
}
```

```haskell
scheme :: Int -> (Int -> Int -> Int) -> Int -> Int
scheme baseCase recCase n = aux baseCase 1
  where
    aux res i = if i <= n
                then aux (recCase i res) (i + 1)
                else res
```

Note that the scheme is usually simpler to write as it only focuses on recursion, not
business logic. Furthermore one scheme may fit many functions thus reducing the complexity
and bugs in writing business functions. Remember that `fact` and `sum` are purposely trivial.
They are just toy example to introduce the subject. In practice you will use much more complex
recursive functions. Once you've understood this example, you'll be able to scale this technique
to any recursive one, however complex it is.

## Scaling up!

To be sure we have a good understanding of the techniqe, let's apply it to the *fibonacci* function we all love.
It is defined on *non-negative* integers by

$$fib(0) = 1$$
$$fib(1) = 1$$
$$fib(n+2) = fib(n+1) + fib(n)$$


```scala
def fib(n: Int): Int =
  n match {
    case 0 => 1
    case 1 => 1
    case n =>
      val r1 = fib(n-1)
      val r2 = fib(n-2)
      r1 + r2
  }
```

```haskell
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = r1 + r2
  where
    r1 = fib (n - 1)
    r2 = fib (n - 2)
```

The function `fib` does not call itself when `n` is *0* or *1* but calls itself twice, on `n-1` and `n-2` otherwise.
So we can, like `fact` and `sum`, split `fib` into two pieces: one containing only recursion-relevant code and the
other one the rest. Once again the split is done by taking recursion-irrelevant code out of the function's body.
Remember they are many ways to split it up. This one is just one of many sensible way of doing so:

```scala
/* Part irrelevant to recursion:
 * The real definitions of these variables
 * have no impact on how fact is calling itself
 */
val baseCase0: Int = 1
val baseCase1: Int = 1
def recCase(r1: Int, r2: Int): Int = r1 + r2

/* Recursion-only part:
 * The only implementation details it contains
 * are about how fib it calling itself
 */
def fib(n: Int): Int =
  n match {
    case 0 => baseCase0
    case 1 => baseCase1
    case n =>
      val r1 = fib(n-1)
      val r2 = fib(n-2)
      recCase(r1, r2)
  }
```

```haskell
{-
 Part irrelevant to recursion:
 The real definitions of these variables
 Have no impact on how fact is calling itself
-}
baseCase0 :: Int
baseCase0 = 1

baseCase1 :: Int
baseCase1 = 1

recCase :: Int -> Int -> Int
recCase n r = n + r

{-
 Recursion-only part:
 The only implementation details it contains
 are about how fact it calling itself
 -}
fib :: Int -> Int
fib 0 = baseCase0
fib 1 = baseCase1
fib n = recCase r1 r2
  where
    r1 = fib (n - 1)
    r2 = fib (n - 2)
```

Which leads to the recursion scheme:

```scala
def scheme(baseCase0: Int, baseCase1: Int, recCase: (Int, Int) => Int): Int => Int = {
  def commonRecursiveRelevantPart(n: Int): Int =
    n match {
      case 0 => baseCase0
      case 1 => baseCase1
      case n =>
        val r1 = commonRecursiveRelevantPart(n - 1)
        val r2 = commonRecursiveRelevantPart(n - 2)
        recCase(r1, r2)
    }

  commonRecursiveRelevantPart
}
```

```haskell
scheme :: Int -> Int -> (Int -> Int -> Int) -> Int -> Int
scheme baseCase0 baseCase1 recCase = aux
  where
    aux 0 = baseCase0
    aux 1 = baseCase1
    aux n = r1 + r2
      where
        r1 = aux (n - 1)
        r2 = aux (n - 2)
```

It is then trivial to define `fib` by giving appropriate definition to `scheme` arguments: `baseCase0`, `baseCase1` and
`recCase`.

```scala
def fib: Int => Int = scheme(1, 1, (r1: Int, r2: Int) => r1 + r2)
```

```haskell
fib :: Int -> Int
fib = scheme 1 1 (+)
```

Once again this implementation is not optimal as each call of `fib` can make to 2 recursive calls
which leads to an exponential time complexity. While computing `fib(5)` is fast, computing `fib(1000)` may
take much longer. As you already probably guessed, writing the recursion scheme as an iterative loop, which
sadly makes it more intricate, solves the problem:

```scala
def scheme(baseCase0: Int, baseCase1: Int, recCase: (Int, Int) => Int)(n: Int): Int =
  if (n == 0) baseCase0
  else {
    var b0 = baseCase0
    var b1 = baseCase1
    var i = 2
    while (i <= n) {
      val b2 = recCase(b0, b1)
      b0 = b1
      b1 = b2
      i += 1
    }
    b1
  }
```

```haskell
scheme :: Int -> Int -> (Int -> Int -> Int) -> Int -> Int
scheme baseCase0 baseCase1 recCase 0 = baseCase0
scheme baseCase0 baseCase1 recCase n = aux baseCase0 baseCase1 2
  where
    aux b0 b1 i = if i <= n
                  then aux b1 (recCase b0 b1) (i + 1)
                  else b1
```

By now you should get a good grasp on what recursion schemes are. But we have
only seen a tiny fraction of how useful they are. It's about time to consider
the real power of `fact`, `sum` and `fib`'s schemes.

## Time to take off!

Previously we defined `fact` and `sum`'s schemes as

```scala
def scheme(baseCase: Int, recCase: (Int, Int) => Int)(n: Int): Int = {
  var res = baseCase
  var i: Int = 1
  while (i <= n) {
    res = recCase(i, res)
    i += 1
  }
  res
}
```

```haskell
scheme :: Int -> (Int -> Int -> Int) -> Int -> Int
scheme baseCase recCase n = aux baseCase 1
  where
    aux res i = if i <= n
                then aux (recCase i res) (i + 1)
                else res
```

I have a small exercise for you: find where this code relies on `baseCase` to be an `Int`?
It's important, take the time to figure it out. The answer is simple: it does not! `baseCase` can actually be
any of type `A`! We don't even have to modify the code (only the type signature):

```scala
def scheme[A](baseCase: A, recCase: (Int, A) => A)(n: Int): A = {
  var res = baseCase
  var i: Int = 1
  while (i <= n) {
    res = recCase(i, res)
    i += 1
  }
  res
}
```

```haskell
scheme :: a -> (Int -> a -> a) -> Int -> a
scheme baseCase recCase n = aux baseCase 1
  where
    aux res i = if i <= n
                then aux (recCase i res) (i + 1)
                else res
```

Not only can we still define `fact` (and `sum`) like above but it makes trivial defining
the functions like `list` which returns the list of integers between *1* and *n*:

```scala
def list: Int => List[Int] = scheme[List[Int]](Nil, (n: Int, r: List[Int]) => n :: r)
```

```haskell
list :: Int -> [Int]
list = scheme [] (:)
```

Unsurprisingly `fib`'s recursion scheme can also be generalized without changing a single line of code (only type signature):

```scala
def scheme[A](baseCase0: A, baseCase1: A, recCase: (A, A) => A)(n: Int): A =
  if (n == 0) baseCase0
  else {
    var b0 = baseCase0
    var b1 = baseCase1
    var i = 2
    while (i <= n) {
      val b2 = recCase(b0, b1)
      b0 = b1
      b1 = b2
      i += 1
    }
    b1
  }
```

```haskell
scheme :: a -> a -> (a -> a -> a) -> Int -> a
scheme baseCase0 baseCase1 recCase 0 = baseCase0
scheme baseCase0 baseCase1 recCase n = aux baseCase0 baseCase1 2
  where
    aux b0 b1 i = if i <= n
                  then aux b1 (recCase b0 b1) (i + 1)
                  else b1
```

While `fact`'s scheme is related to lists, `fib`'s one is related to trees:

```scala
sealed abstract class Tree[+A]
final case class Leaf[+A](value: A) extends Tree[A]
final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def tree: Int => Tree[Boolean] =
  scheme(
    Leaf(false),
    Leaf(true),
    (r1: Tree[Boolean], r2: Tree[Boolean]) => Node(r1,r2)
  )
```

```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)

tree :: Int -> Tree Bool
tree = scheme (Leaf False) (Leaf True) Node
```

I have few real exercises for you this time:

- *find in your production code several spots where this scheme could be useful.*
- *write schemes, as general as possible, for at least 5 recursive functions in our production code.*

Obviously I won't check you did the exercises but you should really do them. Reading is not sufficient to
develop your understanding of the technique, you need to experiment! Try things, play with
these notions until it clicks. **Learning recursion schemes is like going on expeditions:
preparation time may seem the easier part but if you did not prepare well enough, you'll get lost.**

## Yeah! Buzzwords!

As we have seen, `fact`'s scheme takes 2 arguments:

```scala
def scheme[A](baseCase: A, recCase: (Int, A) => A): Int => A
```

```haskell
scheme :: a -> (Int -> a -> a) -> Int -> a
```

While this definition is perfectly ok, we can regroup these argument in any structure that can hold both values like
a pair, an interface or a trait:

```scala
trait FactorialSchemeArguments[A] {
  val baseCase: A
  def recCase(n: Int, r: A): A
}

def scheme[A](arguments: FactorialSchemeArguments[A]): Int => A
```

```haskell
class FactorialSchemeArguments a where
  baseCase :: a
  recCase :: Int -> a -> a

scheme :: FactorialSchemeArguments a => Int -> a
```

Note that `scheme` is still the same: it still takes the same two arguments.
But even if the code didn't change, this transformation makes us see `scheme`
from a different perspective. It shows `scheme` as a functions transforming
an integer to an `A` provided that we give some structure to  `A`: a constant
`baseCase` and an operation `recCase`. Let's give this structure and the scheme
names: i decide to call the structure a *AkolovioaAlgebra* (don't look for
it in literature, i just coined the term) and the scheme an *akolovioaMorphism*:

```scala
trait AkolovioaAlgebra[A] {
  val initial: A
  def action(n: Int, r: A): A
}

def akolovioaMorphism[A: AkolovioaAlgebra]: Int => A
```

```haskell
class AkolovioaAlgebra a where
  initial :: a
  action :: Int -> a -> a

akolovioaMorphism :: AkolovioaAlgebra a => Int -> a
```

This looks smart, doesn't it? 😉 It is actually very close to a very common structure in
programming! *Will you find which one?* Obviously the same can be done for *fibonacci*'s scheme.
*As an exercise, apply this technique to fibonacci's scheme and give them pretty names.*

## Where to go from here?

As you know this is not the end of the story: the subject is closely related to
pervasive notions such as (co)algebras, inductive types, categories, initial-objects, fixed-points,
algebraic data types, etc. Whichever next subject you choose to dive into, the approach this article follows,
i.e. experimenting on toy examples, really helps developing a solid understanding. I want you to realize
each definition you read in books, articles, talks, etc is the result of people experimenting. The common trap
in this field is looking at definitions as sacred pieces of unquestionable truth no mortal can see through.
It is actually the exact opposite! Science is by essence experimentation. This is by investigating and trying things
you end up figuring out how things work. But, like in science, for your investigation to be productive your tests need
to be done in a controlled environment with as few variables as possible so that it is easy for you to see what's going on.
That's why toy examples are so important: they contain the essence of what makes things work without all the noise
real examples have.

Take care and spread recursion schemes around 😉
