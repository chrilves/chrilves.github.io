---
title: "Gadts By Use Cases"
date: 2019-09-26T11:58:02+02:00
draft: true
description: 
keywords:
  - GATD
  - GADTs
  - Generalized Algebraic Data Types
  - ADT
  - Algebraic Data Types
  - Functional Programming
  - Interface
  - Free Monad
---

## Assumptions

**In this presentation we will assume that:**

- `null` **does not exists!**
- **runtime reflection does not exist!**

# Stretching

## *Values* and *Types* ?

**Values** are *actual piece of data your program manipulates* like the integer `5`, the boolean `true`,
the string `"Hello World!"`, the function `(x: Double) => x / 7.5`, the list `List(1,2,3)`, etc.
We often like to classify values into groups. These groups are called **types**. For example:

- `Int` is the group of integer values, i.e. values like `1`, `-7`, `19`, etc.
- `Boolean` is the group containing exactly the values `true` and `false` (no more, no less!).
- `String` is the group whose values are `"Hello World!"`, `""`, `"I ❤️ GADTs"`, etc.
- `Double => Double` is the group whose values are functions taking any `Double` as argument and returning some `Double`.

To indicate that the value `v` belongs to the type (group of values) `T`, we write `v : T`.
In *Scala*, testing if a value `v` belongs to a type `T` is very simple: just type `v : T` in the *REPL*:

```scala
scala> 5 : Int
res7: Int = 5
```

If *Scala* accepts it, then `v` belongs to `T`. If *Scala* complains, it most probably does not :

```scala
scala> 5 : String
       ^
       error: type mismatch;
        found   : Int(5)
        required: String
```


## How many types?

Let's now create some types and some of their values (when possible!). 

```scala
class OneType
```

1. How many types does the line `class OneType` defines?

    <details>
      <summary>*Solution (click to expand)*</summary>
      
      As the name suggests, `class OneType` defines only one type which is the *class* `OneType`.
    </details>

Let's now consider:

```scala
class OneTypeForEvery[A]
```

2. How many types does the line `class OneTypeForEvery[A]` defines?

    <details>
      <summary>*Solution (click to expand)*</summary>
      
      As the name suggests, every concrete type `A` give rise to a distinct type `OneTypeForEvery[A]`.
      
      For example, a list of integers is neither a list of booleans, nor a list of strings,
      nor a list of functions, nor ... It means the types `List[Int]`, `List[Boolean]`,
      `List[Int => Int]`, etc are all distinct types.

      The line `class OneTypeForEvery[A]` defines a type **for every concrete type** `A`. There
      is an infnity of types `A`, so an infinity of types `OneTypeForEvery[A]`. Very much list
      there is an infinity of sorts of lists.
    </details>

3. Give a value that belongs to both `OneTypeForEvery[Int]` and `OneTypeForEvery[Boolean]`.

    **Remember that `null` does not exist!**

    <details>
      <summary>*Solution (click to expand)*</summary>
      This is actually impossible. Every concrete type `A` give rise a distinct type `OneTypeForEvery[A]`
      that have no values in common with others types `OneTypeForEvery[B]` for `B ≠ A`.
    </details>

## How many values?

Considering the following type:

```scala
final abstract class NoValueForThisType
```

1. Give a value belonging to the type `NoValueForThisType`?
   How many values belong to `NoValueForThisType`?

    <details>
      <summary>*Hint (click to expand)*</summary>  
      - What is a `final` class? How does it differ from a normal non-final class?
      - What is an `abstract` class? How does it differ from a concrete class?
    </details>

    <details>
      <summary>*Solution (click to expand)*</summary>
      
      The class `NoValueForThisType` is declared `abstract`. It is then forbidden to create an
      instance of this class:

      ```scala
      scala> new NoValueForThisType
             ^
             error: class NoValueForThisType is abstract; cannot be instantiated
      ```

      The only way to create an instance of an abstract class is creating a concrete sub-class.
      But the keyword `final` forbids creating such sub-classes:

      ```scala
      scala> class ConcreateSubClass extends NoValueForThisType
                                             ^
             error: illegal inheritance from final class NoValueForThisType
      ```

      There is no way to create an instance of `NoValueForThisType`.
    </details>


Let's take another example:

```scala
sealed trait ExactlyOneValue
case object TheOnlyValue extends ExactlyOneValue
```

2. Give a value belonging to the type `ExactlyOneValue`?

    <details>
      <summary>*Solution (click to expand)*</summary>
      
      By definition, `TheOnlyValue` is a value of type `ExactlyOneValue`.
    </details>

3. How many values belong to `ExactlyOneValue`?

    <details>
      <summary>*Solution (click to expand)*</summary>
      
      Just like above, `ExactlyOneValue` being a `trait` is *abstract*. Being `sealed`,
      extending it outside of its defining file is forbidden.
      So `TheOnlyValue` is the only value of type `ExactlyOneValue`.
    </details>


# First Contact

## Use Case: Evidence of some property

Let's define a simple *sealed trait*:

```scala
sealed trait ATrait[A]
case object AValue extends ATrait[Char]
```

1. Give a value of type `ATrait[Char]`.

    <details>
      <summary>*Solution (click to expand)*</summary>
      
      By definition, `AValue` is a value of type `ATrait[Char]`.
    </details>

2. Give a value of type `ATrait[Int]`.

    <details>
      <summary>*Solution (click to expand)*</summary>
      
      There is no way to have an instance of type `ATrait[Int]`.
      There is actually no way to have an instance of type `ATrait[B]` for `B ≠ Char`
      because the only possible value is `AValue` which is of type `ATrait[Char]`.
    </details>

3. What can you conclude about a type `B` if you have a value of type `ATrait[B]`?

    <details>
      <summary>*Solution (click to expand)*</summary>
      
      The only possible value is `AValue` which is of type `ATrait[Char]` so `B = Char`.
    </details>

4. In the *REPL*, enter the following code

    ```scala
    def f[A](x: A, ev: ATrait[A]): Char =
      x
    ```

5. Now try pattern matching on `ev: ATrait[A]`

    ```scala
    def f[A](x: A, ev: ATrait[A]): Char =
      ev match {
        case AValue => x
      }
    ```

    Is the pattern-matching exhaustive?

    <details>
      <summary>*Solution (click to expand)*</summary>
      
      The pattern-matching is exhaustive because the only possible actual value for `ev` is `AValue`.
      Furthermore `AValue` is of type `ATrait[Char]` which means `v : ATrait[Char]` because `v == AValue`.
      So `A = Char` and `x : Char`.
    </details>

6. Call `f` with `x = 'w' : Char`.

    <details>
      <summary>*Solution (click to expand)*</summary>
      
      ```scala
      scala> f[Char]('w', AValue)
      res0: Char = w
      ```
    </details>

7. Call `f` with `x =  5 : Int`.

    <details>
      <summary>*Solution (click to expand)*</summary>

      This is impossible because it would require to give a value `ev : ATrait[Int]` which does not exist!

      ```scala
      scala> f[Int](5, AValue)
                       ^
             error: type mismatch;
               found   : AValue.type
               required: ATrait[Int]
      ```

    </details>

Using all the nice syntactic features of *Scala*, the production-ready version of the code above is:

```scala
sealed trait IsChar[A]
object IsChar {
  implicit case object Evidence extends IsChar[Char]

  def apply[A](implicit evidence: IsChar[A]): IsChar[A] = evidence
}

def f[A: IsChar](x: A): Char =
  IsChar[A] match {
    case IsChar.Evidence => x
  }
```

## Use-Case: The only thing i know is it exists.

What would you do if you wanted your codebase to log messages, but you want to be sure your
codebase do not rely on any implementation details of the logger so that you can change its
implementation without risking breaking the codebase?

Take the following logger type `UnknownLogger`:

```scala
sealed trait UnknownLogger
final case class LogWith[X](logs : X, appendMessage: (X, String) => X) extends UnknownLogger
```

The first logger we will create stores the logs in a `String`:

```scala
val loggerStr : UnknownLogger = LogWith[String]("", (logs: String, message: String) => logs ++ message)
```

The second logger stores them in a `List[String]`:

```scala
val loggerList : UnknownLogger = LogWith[List[String]](Nil, (logs: List[String], message: String) => message :: logs)
```

The third logger directly print the messages to the standard output:

```scala
val loggerStdout : UnknownLogger = LogWith[Unit]((), (logs: Unit, message: String) => println(message))
```

Note that these three loggers all have the same type (i.e. `UnknownLogger`) but they stores
the logs using different types `X` (`String`, `List[String]` and `Unit`). 


1. Let `v` be a value of type `UnknownLogger`. Clearly `v` has to be an instance of type
   `LogWith[X]` for some type `X`. What do you know about `X`? Why is it useful to
   ignore what `X` is?

    **Remember we refuse to use runtime reflection!**

    <details>
      <summary>*Solution (click to expand)*</summary>

    We know almost nothing about `X`. The only thing we know is there exists at least one value (`v.logs`) of type `X`.
    Appart from that `X` can be any type.

    Not knowing which type `X` really is very useful to garantee that the code that will use `v : UnknownLogger` will not
    rely on the nature of `X`. If the code knew `X` was `String` for example, it could perform some operations we want to forbid like
    reversing the list, taking only the nth first characters, etc. By hiding the real type `X`, we force our codebase to
    not depend on what `X` is but to use the provided `v.appendMessage`. So changing the real implementation of the logger
    won't break any code.

    </details>

2. Write the function `def log(message: String, v: UnknownLogger): UnknownLogger` that use `v.appendMessage` to append `message` to `v.logs`
   and returns a new `UnknownLogger` containing the new logs.

    <details>
      <summary>*Solution (click to expand)*</summary>

      ```scala
      def log(message: String, v: UnknownLogger): UnknownLogger =
        v match {
          case vx : LogWith[x] => LogWith[x](vx.appendMessage(vx.logs, message), vx.appendMessage)
        }
      ```

    </details>

3. Execute `log("Hello World", loggerStr)` and `log("Hello World", loggerList)` and `log("Hello World", loggerStdout)`

    <details>
      <summary>*Solution (click to expand)*</summary>

      ```scala
      scala> log("Hello World", loggerStr)
      res0: UnknownLogger = LogWith(Hello World,$$Lambda$988/1455466014@421ead7e)

      scala> log("Hello World", loggerList)
      res1: UnknownLogger = LogWith(List(Hello World),$$Lambda$989/1705282731@655621fd)

      scala> log("Hello World", loggerStdout)
      Hello World
      res2: UnknownLogger = LogWith((),$$Lambda$990/1835105031@340c57e0)
      ```

    </details>


Another real-life example would be:

```scala
trait Writer[A] {
  def writes(a:A): String
}

sealed trait Serializable[A] {
  type tpe
  def value: tpe
  implicit val writer: Writer[tpe]
}
object Serializable {
  final case class Evidence[A](value: A, writer: Writer[A]) extends Serializable[A] {
    type tpe = A
  }

  def apply[A](thevalue: A)(implicit w : Writer[A]): Serializable[A] =
    Evidence(thevalue, w)
}
```

## Conclusion

*GADTs* are actually only this: simple *sealed trait* with some *case object* (possibly none) and
some *case class* (possible none too!). In the following parts we will explore some major use cases of
*GATDs*

# Easy Useful Use Cases:<br/>Relations on Types

One easy but very useful benefit of *GADTs* is expressing relations about types such that:

- Is type `A` equal to type `B`?
- Is type `A` a sub-type of `B`?

Note that, by definition, a type `A` is always considered a sub-type of itself (i.e. `A <: A`),
very much like an integer `x` is also considered lesser-than-or-equal to itself `x ≤ x`.

## Use Case: Witnessing Type Equality

```scala
sealed trait EqT[A,B]
final case class Evidence[X]() extends EqT[X,X]
```

1. Give a value of type `EqT[Int, Int]`

    <details>
      <summary>*Solution (click to expand)*</summary>

      ```scala
      scala> Evidence[Int]() : EqT[Int, Int]
      res0: EqT[Int,Int] = Evidence()
      ```

    </details>

2. Give a value of type `EqT[String, Int]`

    <details>
      <summary>*Solution (click to expand)*</summary>

      The class `Evidence` is the only concrete sub-class of trait `EqT` and we cannot
      create another one because `EqT` is `sealed`. So any value `v : EqT[A,B]` has to be
      an instance of `Evidence[X]` for some type `X`, which is of type `EqT[X,X]`.
      Thus there is no way to get a value of type `EqT[String, Int]`

    </details>

3. Given two (unknown) types `A` and `B`.
   What can you conclude if i give you a value of type `EqT[A,B]`?

    <details>
      <summary>*Solution (click to expand)*</summary>

      If i give you a value `v : EqT[A,B]`, then you know that `v` is an instance of 
      `Evidence[X]` for some type `X` because the class `Evidence` is the only concrete
      sub-class of the sealed trait `EqT`. Actually `Evidence[X]` is a sub-type of
      `EqT[X,X]`. Thus `v : EqT[X,X]`. Types `EqT[A,B]` and `EqT[X,X]` have no value in
      common if `A ≠ X` or `B ≠ X`, so `A = X` and `B = X`. Thus `A = B`.

    </details>

In production, it is convenient to define the following equivalent code:

```scala
sealed trait EqT[A,B]
object EqT {
  final case class Evidence[X]() extends EqT[X,X]

  implicit def evidence[X] : EqT[X,X] = Evidence[X]()

  def apply[A,B](implicit ev: EqT[A,B]): ev.type = ev
}
```

## Use Case: Conversions between Equal Types

If `A` and `B` are actually the same type, then `List[A]` is also the
same type as `List[B]`, `Option[A]` is also the same type as `Option[B]`,
etc. More generally, for any `F[_]`, `F[A]` is also the same type as `F[B]`.

1. Write the function `toF[F[_],A,B](eqT: EqT[A,B])(fa: F[A]): F[B]`.
2. Using the function `toF` above, write the function `toScalaEq[A,B](eqT: EqT[A,B]): A =:= B`.


## Use Case: Witnessing Sub Typing

1. Create the type `SubTypeOf[A,B]` (and all that is necessary) such that:

    > There exists a value of type `SubType[A,B]` **if and only if** `A` is a sub-type of `B` (i.e. `A <: B`).

    Remember that, by definition, a type `A` is always considered a sub-type of itself (i.e. `A <: A`).

## Use Case: Avoiding annoying *scalac* error messages about bounds not respected

In this example, we want to model the diet of some animals. We start by defining the `Food` type and some of its subtypes:

```scala
trait Food
class Vegetable extends Food
class Fruit extends Food
```

and then the class representing animals eating food of type `A` (i.e. `Vegetable`, `Fruit`, etc):

```scala
class AnimalEating[A <: Food]

val elephant : AnimalEating[Vegetable] = new AnimalEating[Vegetable]
```

Let's define a function like there are so many in *Functional Programming* and apply it to `elephant`:

```scala
def dummy[F[_],A](fa: F[A]): Unit = ()
```

```scala
scala> dummy(elephant)
       ^
       error: inferred kinds of the type arguments (AnimalEating,Vegetable) do not conform to the expected kinds of the type parameters (type F,type A).
       AnimalEating's type parameters do not match type F's expected parameters:
       type A's bounds <: Food are stricter than type _'s declared bounds >: Nothing <: Any
             ^
       error: type mismatch;
        found   : AnimalEating[Vegetable]
        required: F[A]
```

1. Why does *scalac* complains?

The problem is that, when we defined `class AnimalEating[A <: Food]`,
we gave the restriction that `A <: Food`. So *Scala*, like *Java*,
forbids us to give `AnimalEating` anything but a sub-type of `Food`
(including `Food` itself):

```scala
scala> type T1 = AnimalEating[Int]
                 ^
       error: type arguments [Int] do not conform to class AnimalEating's type parameter bounds [A <: Food]

scala> type T2 = AnimalEating[Food]
defined type alias T2
```

We face a dilemma: to use the function `dummy`, that we really want to use because it's a very nice function,
we need to remove the constraint `A <: Food` from the definition of `class AnimalEating[A <: Food]`.
But we still want to say that animals eat food, not integers, boolean or strings!

1. How can you adapt the definition `class AnimalEating[A]` so that:

    > There exists a value of type `AnimalEating[A]` **if and only if** `A` is a sub-type of `Food` (i.e. `A <: Food`).

# Standard Use Cases

## Use Case: Effects!

What we call an effect is sometimes just an interface declaring some functions with
no implementation. For example we can define this trait (note that the function `echo` has
no implementation):

```scala
trait EffectSig {
  def echo[A](value: A): A
  def randomInt() : Int
  def ignore[A](value: A): Unit
}
```

Implementations of these interfaces are given elsewhere and there can be many of them! This
is useful to switch between implementations easily :

```scala
object EffectImpl extends EffectSig {
  def echo[A](value: A): A = value
  def randomInt() : Int = scala.util.Random.nextInt()
  def ignore[A](value: A): Unit = ()
}
```

Another equivalent way to define effects is via:

```scala
sealed trait Effect[A]
final case class Echo[A](value: A) extends Effect[A]
final case class RandomInt() extends Effect[Int]
final case class Ignore[A](value: A) extends Effect[Unit]
```

Once again this is a declaration with no implementation! Once again implementations
can be written elsewhere and there can also be many of them:

```scala
def runEffect[A](effect: Effect[A]): A =
  effect match {
    case Echo(value) => value
    case RandomInt() => scala.util.Random.nextInt()
    case Ignore(_) => ()
  }
```

Let's consider a more realistic effect and one possible implementation:

```scala
trait EffectSig {
  def currentTimeMillis: Long
  def printLn(msg: String): Unit
  def mesure[X,A](fun: X => A, arg: X): A
}

object EffectImpl extends EffectSig {
  def currentTimeMillis: Long = System.currentTimeMillis()
  def printLn(msg: String): Unit = println(msg)
  def mesure[X,A](fun: X => A, arg: X): A = {
    val t0 = System.currentTimeMillis()
    val r  = fun(arg)
    val t1 = System.currentTimeMillis()
    println(s"Took ${t1 - t0} milli-seconds")
    r
  }
}
```

1. Write the **GADT** `Effect[A]` representing the trait `EffectSig`.
2. Write the function `run[A](effect: Effect[A]): A` implementing the effect like `EffectImpl` does.

The *GADT* `Effect[A]` declare interesting effects (`CurrentTimeMillis`, `PrintLn` and `Mesure`) but,
to be useful we want it to support the following operations:

- `def pure[A](value: A): Effect[A]`
- `def flatMap[X,A](fa: Effect[X], f: X => Effect[A]): Effect[A]`

3. Add two *case classes*, `Pure` and `FlatMap`, to the definition of the *GADT* `Effect[A]` encoding these operations.
4. Adapt the function `run` to handle these two new cases.
5. Add the two following methods to trait `Effect[A]` to get:

    ```scala
    sealed trait Effect[A] {
      final def flatMap[B](f: A => Effect[B]): Effect[B] = FlatMap(this, f)
      final def map[B](f: A => B): Effect[B] = flatMap[B]((a:A) => Pure(f(a)))
    }
    ```

    And run the follwing code to see if it works:

    ```scala
    val effect1: Effect[Unit] =
      for {
        t0 <- CurrentTimeMillis
        _  <- PrintLn(s"The current time is $t0")
      } yield ()

    run(effect1)
    ```

## Use Case: Simplifying Implicits

Heterogeneous lists are lists whose element can be of different types. They are usually defined in *Scala*
almost like normal lists:

```scala
final case class HNil() // The empty list
final case class HCons[Head,Tail](head: Head, tail: Tail) // The `head :: tail` operation

val empty : HNil =
  HNil()

val oneTrueToto : HCons[Int, HCons[Boolean, HCons[String, HNil]]] =
  HCons(1, HCons(true, HCons("toto", HNil())))
```

As you can see, there is nothing special about it. We want to define orderings on heterogeneous lists.
An ordering is a way to compare two values (**of the same type!**): they can be equal or one may
be lesser than the other. In *Scala* we can define the trait `Order`:

```scala
trait Order[A] {
  // true if and only if a1 < a2
  def lesserThan(a1: A, a2: A): Boolean

  // a1 and a2 are equal if and only if none of them is lesser than the other.
  final def areEqual(a1: A, a2: A): Boolean = !lesserThan(a1, a2) && !lesserThan(a2, a1)

  // a1 > a2 are if and only if a2 < a1
  final def greaterThan(a1: A, a2: A): Boolean = lesserThan(a2, a1)

  final def lesserThanOrEqual(a1: A, a2: A): Boolean = !lesserThan(a2, a1)

  final def greaterThanOrEqual(a1: A, a2: A): Boolean = !lesserThan(a1, a2)
}

object Order {
  def apply[A](implicit ev: Order[A]): ev.type = ev

  def make[A](lg_ : (A,A) => Boolean): Order[A] =
    new Order[A] {
      def lesserThan(a1: A, a2: A): Boolean = lg_(a1,a2)
    }
}

implicit val orderInt    = Order.make[Int](_ < _)
implicit val orderString = Order.make[String](_ < _)
```

Remember that we will only compare lists of the same type:

- Lists of type `HNil` will only be compared to lists of type `HNil`.
- Lists of type `HCons[H,T]` will only be compared to lists of type `HCons[H,T]`.

Comparing lists of type `HNil` is trivial because there is only one value
of type `HNil`: the empty list `HNil()`. We want to define several orderings
on heterogeneous lists:

- The lexicographic ordering (i.e. dictionary order: from left to right)

    > `HCons(h1,t1) < HCons(h2,t2)` **if and only if** `h1 < h2` *or* (`h1 == h2` *and* `t1 < t2` *by lexicographic ordering*).
    
    ```scala
    sealed trait Lex[A] {
      val order : Order[A]
    }

    object Lex {
      def apply[A](implicit ev: Lex[A]): ev.type = ev

      implicit val lexHNil: Lex[HNil] =
        new Lex[HNil] {
          val order = Order.make[HNil]((_,_) => false)
        }

      implicit def lexHCons[Head,Tail](implicit
          orderHead: Order[Head],
          lexTail: Lex[Tail]
        ): Lex[HCons[Head, Tail]] =
        new Lex[HCons[Head, Tail]] {
          val orderTail: Order[Tail] = lexTail.order

          val order = Order.make[HCons[Head, Tail]] {
            case (HCons(h1,t1), HCons(h2,t2)) =>
              orderHead.lesserThan(h1,h2) || (orderHead.areEqual(h1,h2) && orderTail.lesserThan(t1,t2))
          }
        }
    }
    ```

- The reverse-lexicographic ordering which is the reverse version of the lexicographic
  ordering (i.e. from right to left)

    > `HCons(h1,t1) < HCons(h2,t2)` **if and only if** `t1 < t2` *by reverse-lexicographic ordering or* (`t1 == t2` *and* `h1 < h2`).

    ```scala
    sealed trait RevLex[A] {
      val order : Order[A]
    }

    object RevLex {
      def apply[A](implicit ev: RevLex[A]): ev.type = ev

      implicit val revLexHNil: Lex[HNil] =
        new Lex[HNil] {
          val order = Order.make[HNil]((_,_) => false)
        }

      implicit def revLexHCons[Head,Tail](implicit
          orderHead: Order[Head],
          revLexTail: Lex[Tail]
        ): RevLex[HCons[Head, Tail]] =
        new RevLex[HCons[Head, Tail]] {
          val orderTail: Order[Tail] = revLexTail.order

          val order = Order.make[HCons[Head, Tail]] {
            case (HCons(h1,t1), HCons(h2,t2)) =>
              orderTail.lesserThan(t1,t2) || (orderTail.areEqual(t1,t2) && orderHead.lesserThan(h1,h2))
          }
        }
    }
    ```

1. The `Alternate` ordering is defined by:

    > `HCons(h1,t1) < HCons(h2,t2)` **if and only if** `h1 < h2` *or* (`h1 == h2` *and* `t1 > t2` *by alternate ordering*).

    Just like what was done for `Lex` and `RevLex`, implement the `Alternate` ordering.
    

There are lots of ways to define a valid ordering on heterogeneous lists! Defining type-classes like `Lex`, `RevLex` and `Alternate` for every ordering we want to implement is clunky and messy. We can do much better than that ... with a *GADT* ;)

```scala
sealed trait HListOrder[A]
object HListOrder {
  final case object HNilOrder extends HListOrder[HNil]

  final case class HConsOrder[Head,Tail](
      orderHead: Order[Head],
      hlistOrderTail: HListOrder[Tail]
    ) extends HListOrder[HCons[Head,Tail]]

  // Implicit definitions

  implicit val hnilOrder : HListOrder[HNil] =
    HNilOrder

  implicit def hconsOrder[Head,Tail](implicit
      orderHead: Order[Head],
      hlistOrderTail: HListOrder[Tail]
    ): HListOrder[HCons[Head,Tail]] =
    HConsOrder(orderHead, hlistOrderTail)

  def apply[A](implicit ev: HListOrder[A]): ev.type = ev
}
```

Note that these implicit definitions are boilerplate. Their only purpose is passing arguments
to their corresponding constructor (i.e. `case class` or `case object`):
`hnilOrder` to `HListOrder` (O arguments) and `hconsOrder` to `HConsOrder` (2 arguments).

2. Write the function `def lex[A: HListOrder] : Order[A]` that compute the lexicographic ordering from a value of type `HListOrder[A]]`.
3. Write the function `def revLex[A: HListOrder] : Order[A]` that compute the reverse-lexicographic ordering from a value of type `HListOrder[A]]`.

# A More Advanced Use Case:<br/>Ensuring types are supported by the Database

In this section we consider the use case of manipulating data that came from a database.
Our database **only** supports the following types:

- `String`
- `(A,B)` where `A` and `B` are also types supported by the database.
- Any **unkown** sub-type of [java.lang.Number](https://docs.oracle.com/javase/7/docs/api/java/lang/Number.html).
    It should be impossible to know which sub-type of `Number` is used, only that an unknown sub-type of `Number` was.

1. Define the type `DBType[A]` such that:

    > There exists a value of type `DBType[A]` **if and only if** `A` is a type supported by the database.

Using `DBType`, we can pair a value of type `A` with a value of type `DBType[A]` which provides an evidence
that the type `A` is supported by the database:

```scala
final case class DBValue[A](value: A)(implicit val dbType: DBType[A])
```

A functor is, approximately, a type constructor `F[_]` like `List`, `Option`, `DBValue`, ...
for which you can write an instance of the trait

```scala
trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}
```

where `map(fa)(f)` applies the function `f` to any value of type `A` contained in `fa`. For example:

```scala
implicit object OptionFunctor extends Functor[Option] {
  def map[A,B](fa: Option[A])(f: A => B): Option[B] =
    fa match {
      case Some(a) => Some(f(a))
      case None => None
    }
}
```

2. Write an instance of `Functor[DBValue]`.

A *Generalized Functor* is very much like a regular `Functor` but
the `map` function do not apply to any type `A` and `B` but is restricted
to the only types `A` and `B` satisfying some condition:

```scala
trait GenFunctor[P[_],F[_]] {
  def map[A,B](fa: F[A])(f: A => B)(implicit evA: P[A], evB: P[B]): F[B]
}
```

For example `Set` (more precisely `TreeSet`) is not a functor!
Indeed there is no way to write a function `map` that works for any type `B` (because `B` need to
have an ordering).
But if we restrict `map` to only types `B` having an ordering, then we can write it.

```scala
import scala.collection.immutable._
implicit object TreeSetFunctor extends GenFunctor[Ordering, TreeSet] {
  def map[A,B](fa: TreeSet[A])(f: A => B)(implicit evA: Ordering[A], evB: Ordering[B]): TreeSet[B] =
    TreeSet.empty[B](evB) ++ fa.toSeq.map(f)
}
```

3. Write an instance of `GenFunctor[DBType, DBValue]`r