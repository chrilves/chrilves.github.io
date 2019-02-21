---
title: "Demystifying GADTs"
date: 2019-02-20T11:09:38+01:00
draft: false
description: Introduction to catamorphisms on Algebraic Data Types
keywords:
  - GADT
  - Types
  - Object-Oriented Programming
---

**Generalized Algebraic Data Types (GADT)** is certainly one of the most feared concept in programming nowadays. Very few mainstream languages support GADTs. The only ones i know which does are [Haskell](https://www.haskell.org/ghc), [Scala](https://www.scala-lang.org), [OCaml](https://ocaml.org/index.fr.html) and [Haxe](https://haxe.org). The idea is actually very simple but often presented in complicated ways. In fact, if you're familiar to both basic *Object-Oriented-with-Generics* and basic *functional* programming, then you most probably are already familiar with GADTs without even knowing you are. But if GADTs are so simple, why so many people feel terrified by them? Well GADTs rely on two fundamental ideas, one of them is known by every *Object-Oriented-with-Generics* programmer while the other is known by every *functional* programmer. The problem is most people make the huge mistake of opposing them even though they are complementary. So before diving into GADTs, let me remind you of these elementary notions from *Object-Oriented* and *functional* programming.

## Object-Oriented Programming 101

Let's start by some plain old [Java](https://openjdk.java.net) (the examples works in probably all Object-Oriented language which supports generics). We want to define an *abstract class* for sequences:

```java
public abstract class Sequence<A> {
  abstract public int length();
  abstract public A   getNthElement(int nth);
}
```

In *Java* it would be better to define `Sequence<A>` as an interface but i want this example to be as simple as possible. Would you be surprised if told you a `String` is a sequence of characters ? ;) As i said, GADTs rely on very basic programming knowledge.

```java
public class MyString extends Sequence<Character> {
  private String str;

  public MyString(String s) {
    this.str = s;
  }

  public int length() {
    return this.str.length();
  }

  public Character getNthElement(int nth) {
    return this.str.charAt(nth);
  }
}
```

Likewise, `byte`s are sequences of 8 bits (we represent a bit by a `Boolean`):

```java
public final class MyByte extends Sequence<Boolean> {
  private byte bte;

  public MyByte(byte x) {
    this.bte = x;
  }

  public int length() {
    return 8;
  }

  public Boolean getNthElement(int nth) {
    if (nth >= 0 && nth <= 7)
      return ((bte >>> nth & 1) == 1);
    else
      throw new java.lang.IndexOutOfBoundsException("");
  }
}
```

Have you noticed how `MyByte` and `MyString` declares themselves being respectively a sequence of booleans (`Sequence<Boolean>`) and a sequence of characters (`Sequence<Character>`) but not sequences of `A` (`Sequence<A>`) for any type `A`? Let's try to make it work for any type `A`:

```java
public final class MyByte<A> extends Sequence<A> {
  private byte bte;

  public MyByte(byte x) {
    this.bte = x;
  }

  public int length() {
    ???
  }

  public A getNthElement(int nth) {
    ???
  }
}
```

How would you write the methods `length` and `getNthElement`? Do you really imagine what would be a `MyByte<Graphics2D>`? It just doesn't make any sense at all. You could argue that a string is also a sequence of `byte` and a byte a sequence of one byte. Indeed this relation is not unique, but it does not change the fact that it works for only a small selection of type `A` and not every one! We can go even deeper in *Object-Oriented Programming*:

```java
public final class MyArray<A extends Number> extends Sequence<Number> {
  private A[] array;

  public MyArray(A[] a) {
    this.array = a;
  }

  public int length() {
    return this.array.length;
  }

  public Number getNthElement(int nth) {
    return this.array[nth];
  }
}
```

Note how the generics `A`, which is required to be a sub-class of `Number`, is present as argument of `MyArray` but not in `extends Sequence<Number>`. Now what do you think about this code? Do you think it can be wrong?

```java
public static <A> void guess(Sequence<A> x) {
  if (x instanceof MyByte) {
    System.out.println("I guess A is actually Boolean, let's check!");
    System.out.println(((Sequence<Boolean>)x).getNthElement(0).getClass().getName());
  } else
  if (x instanceof MyString) {
    System.out.println("I guess A is actually Character");
    System.out.println(((Sequence<Character>)x).getNthElement(0).getClass().getName());
  } else
  if (x instanceof MyArray<?>) {
    System.out.println("I guess A is a sub-class of Number but i can not guess which one");
    System.out.println(((Sequence<?>) x).getNthElement(0).getClass().getName());
  }
  else
   System.out.println("I don't know what A is");
}
```

- If `x` is an instance of `MyByte`, which is a sub-class of `Sequence<Boolean>`, then by trivial inheritance `x` is also an instance of `Sequence<Boolean>`. In this case `A` is forced to be `Boolean`.
- If `x` is an instance of `MyString`, which is a sub-class of `Sequence<Character>`, then again by trivial inheritance `x` is also an instance of `Sequence<Character>` . In this case `A` has to be `Character`.
- If `x` is an instance of `MyArray<A>` for some type `A`, which is a sub-class of `Sequence<Number>`, then once again by trivial inheritance `x` is an instance of `Sequence<Number>`. In this case we know `A` is a sub-class of `Number` but we don't know which one.

This is the essence of *Generalized Algebraic Data Types*. It you understand the code above, then you understand how *GADTs* work. As you see this is very basic *Oriented-Object with Generics*. You can find lots of examples of this kind in almost every Java/C#/etc project (search for the `instanceof` keyword).

## Functional Programming 101

Functional languages often support a feature called **Algebraic Data Types (ADT)** which is essentially enumerations on steroids. Like enumerations this is a union of a fixed number of cases but unlike enumerations, where each case is a constant, with ADTs cases can have parameters. As an example, i'll take the type of lists whose elements are of type `a`, written `List a` in *Haskell*. It is defined in *Haskell* by:

```haskell
data List a = Nil | Cons a (List a)
```

It means any value of type `List a` belong to **exactly** one of the following cases:

- either the value is the constant `Nil` which represents the empty list.
- or the value is `Cons hd tl` which represent the list whose first element is `hd` (of type `a`) and whose tail is `tl` (of type `List a`).

The list `[1,2,3,4]` is encoded by `Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))`. Where *ADTs* really shine is pattern-matching which is a very powerful and flexible `switch` (as i said above, *ADTs* are enumerations on steroids). *ADTs* being made of a fixed number of distinct cases, pattern-matching enable to inspect values and perform computations based on a case by case analysis of the form of the value. Here is how to implement the merge sort algorithm on this type:

```haskell
split :: List a -> (List a, List a)
split l =
  case l of
    Cons x (Cons y tl) -> (case split tl of
                              (l1, l2) -> (Cons x l1, Cons y l2)
                          )
    Cons _ Nil         -> (l  , Nil)
    Nil                -> (Nil, Nil)

merge :: (a -> a -> Bool) -> List a -> List a -> List a
merge isLessThan l1 l2 =
  case (l1, l2) of
    (_           , Nil         ) -> l1
    (Nil         , _           ) -> l2
    (Cons hd1 tl1, Cons hd2 _  ) | hd1 `isLessThan` hd2 ->  Cons hd1 (merge isLessThan tl1 l2)
    (_           , Cons hd2 tl2)                        ->  Cons hd2 (merge isLessThan l1 tl2)

sort :: (a -> a -> Bool) -> List a -> List a
sort isLessThan l =
  case l of
    Nil        -> Nil
    Cons _ Nil -> l
    _          -> case split l of
                    (l1, l2) -> merge isLessThan (sort isLessThan l1) (sort isLessThan l2)
```

I know there are smarter ways to write it in *Haskell* but this article is not about *Haskell*. The code above could be translated trivially in `OCaml` by replacing `case ... of` by `match ... with`, in *Scala* by `... match { ... }`, etc. This style is valid is probably all languages supporting pattern-matching so it fits our goal.

The `case l of` expressions are pattern-matching. They are a sequence of `pattern | condition -> code`. The code being executed is the right-hand side of the first case for which the value `l` is of the form of its pattern and satisfy the condition. `l` is then said to match this case. For example, the case `Cons x (Cons y tl) -> (case split tl of (l1, l2) -> (Cons x l1, Cons y l2))` states that if `l` is of the form `Cons x (Cons y tl)`, which means that there are three values `x`, `y` and `tl` such that `l == Cons x (Cons y tl)`, then the code executed is its right-hand side `(case split tl of (l1, l2) -> (Cons x l1, Cons y l2))`. One very important condition is that **pattern-matching must be exhaustive**! It means that the sequence of cases must cover all possible value of `l`.

If your understand the previous section, the type `List a` and how pattern-matching works in the example above, then i am very glad to inform you that you already understand GADTs! Well done :)

## Summing up!

In this section i assume previous sections are ok for you. If you do not understand previous examples, don't go further but go back to the basics of *generics* and *pattern-matching*. Likewise, if you find what follows complicated, go back to the basics *generics* and *pattern-matching*. There is no shame in doing so! Difficulties in understanding advanced notion is often the reflect of a lack of understanding in the ones they rely upon. As i said, there is no shame in it, if you think programming paradigms are "simple" then write a compiler ;)

It's about time to sum up everything. First, note that `List a` is not one single type. Each type `a` actually gives rise to a distinct type `List a`. For example `List Int`, `List String`, `List (List Bool)`, etc are all distinct types. Indeed the list `Cons 1 Nil` is neither a list of strings nor of booleans! For each type `a`, the type `List a` have two constructors: the constant `Nil` and the function `Cons :: a -> List a -> List a` which builds a `List a` from a value of type `a` and other `List a`.

There is another equivalent way to define `List a` in *Haskell* which makes the nature of the constructor more apparent:

```haskell
data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a
```

Indeed, for each type `a`, `Nil` is constant of type `List a` while `Cons` is a function of type `a -> List a -> List a`. Note that it is actually very close to the way to define it in *Scala*:

```scala
sealed abstract class List[A]
final case class Nil[A]() extends List[A]
final case class Cons[A](head: A, tail: List[A]) extends List[A]
```

Do you remember the example of the first section `Sequence<A>`? There was three sub-classes of `Sequence<A`: `MyString` which is actually a sub-class of `Sequence<Character>`, `MyBtyte` which is a sub-class of `Sequence<Boolean>` and `MyArray<A extends Number>` which is a sub-class of `Sequence<Number>`. What is the type of their constructors? Some admissible type for them is (in *Scala* notation):

```haskell
def MyString             : String   => Sequence[Character]
def MyByte               : Byte     => Sequence[Boolean]
def MyArray[A <: Number] : Array[A] => Sequence[Number]
```

From this, this is trivial to write:

```scala
sealed abstract class Sequence[A]
final case class MyString(str: String)                 extends Sequence[Character]
final case class MyByte(bte: Byte)                     extends Sequence[Boolean]
final case class MyArray[A <: Number](array: Array[A]) extends Sequence[Number]
```

or in *Haskell*:

```haskell
data Number where
  MkNum :: forall a. Num a => Number

data Sequence a where
  MyString :: String -> Sequence Char
  MyByte   :: Word8  -> Sequence Bool
  MyArray  :: forall a. Num a => List a -> Sequence Number
```

`Sequence` is a *GADT*. What makes it different from `List` above? For **any type `a`**, values of type `List a` are build using the two constructors `Nil` and `Cons`. Note that it does not depend on what `a` is. Values of type `List Int` are build using the exact same constructors than `List Bool`, `List String`, `List (List Char)`, etc. `Sequence` have three constructors `MyString`, `MyByte` and `MyArray`. But values of type `Sequence[Character]` can only be built by the constructor `MyString` while values of type `Sequence[Boolean]` can only be built by the constructor `MyByte` and values of type `Sequence[Number]` can only be built by the constructor `MyArray`. What about values of type `Sequence[Unit]` or `Sequence[String]`, etc? There is simply no constructor to build values of these types, so *there is no values of these types*!

We can rewrite the methods on `Sequence` and the `guess` function to use patten-matching:

```scala
def length[A](x: Sequence[A]): Int =
  x match {
    case MyByte(_)     => 8
    case MyString(str) => str.length
    case MyArray(arr)  => arr.size
  }

def getNthElement[A](x: Sequence[A], nth: Int): A =
  x match {
    case MyByte(bte) => // So A is actually Boolean
      if (nth >= 0 && nth <= 7)
        (bte >>> nth & 1) == 1
      else
        throw new java.lang.IndexOutOfBoundsException("")

    case MyString(str) => // So A is actually Character
      str.charAt(nth)

    case MyArray(array) => // So A is actually a sub-class of Number
      array(nth)
  }

def guess[A](x : Sequence[A]): Unit =
  x match {
    case MyByte(bte) =>
      println("I guess A is actually Boolean, let's check!")
      println(getNthElement(x, 0).getClass.getName)

    case MyString(str) =>
      println("I guess A is actually Character")
      println(getNthElement(x, 0).getClass.getName)
  
    case MyArray(array) =>
      println("I guess A is a sub-class of Number but i can not guess which one")
      println(getNthElement(x, 0).getClass.getName)
  }
```

As you can see in `getNthElement` must returns a value of type `A` but the case `MyByte` returns a `Boolean`. It means *Scala* is aware that in this case `A` is actually *Boolean*. Likewise in the case `MyString`, *Scala* knowns that the only possible concrete type for `A` is `Character` so it accepts we return one. *Scala* is (most of the time) able to guess, depending on the case, what are the constraints on `A`. This is all the magic behind GADTs: specialized constructors like in *object-oriented-with-generics* programming and closed types (i.e. with a fixed number of cases that we can not extend) on which we can pattern-match like in usual *functional* programming.

How are *GADTs* useful? First of all, there are handy when you have a specialized constructor like in every day life *object-oriented* programming. It makes sense for a byte (resp. string) to be sequence of booleans (resp. characters) but not a sequence of anything. A prolific use of this is writing implicits in Scala as *GADTs*. This way we can pattern-match on the structure of the implicits to derive instances (see [this gist](https://gist.github.com/chrilves/c3db91813cfe693fa708a34f7a27795f) for more details). They are also very useful to encode properties on types. As i said above, not all types `Sequence[A]` have (non-null) values! There is no (non-null) value of type `Sequence[Unit]` or `Sequence[String]` etc but there are values of type `Sequence[Boolean]`, `Sequence[Character]` and `Sequence[Number]`. So if i give you value of type `Sequence[A]`, then you know `A` is either `Boolean`, `Character` or `Number`. If you don't believe me, try to call the function `guess` on a type `A` which neither `Boolean` nor `Character` nor `Number` without using `null`! Let me give you some useful examples.

The first one is restricting a generic type like in:

```scala
sealed abstract class IsIntOrString[A]
implicit final case object IsInt    extends IsIntOrString[Int]
implicit final case object IsString extends IsIntOrString[String]

def canOnlyBeCalledOnIntOrString[A](a: A)(implicit ev: IsIntOrString[A]): A =
  ev match {
    case IsInt => // A is Int
      a + 7
    case IsString => // A is String
      a.reverse
  }

final case class AStringOrAnIntButNothingElse[A](value: A)(implicit val proof : IsIntOrString[A])
```

Another handy use is encoding effects:

```scala
trait UserId
trait User

sealed abstract class BusinessEffect[A]
final case class GetUser(userId: UserId) extends BusinessEffect[User]
final case class SetUser(user: User)     extends BusinessEffect[UserId]
final case class DelUser(userId: UserId) extends BusinessEffect[Unit]
```

Have you ever heard that `Set` is not a functor? With the usual definition of a functor, indeed `Set` is not one.

```scala
trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}
```

The reason is you can only have a `Set[A]` for types `A` such that you can compare values. As an example consider `A == Int => Int`. The two following functions are arguably equals:

```scala
val doubleByMult: Int => Int = (x: Int) => 2 * x
val doubleByPlus: Int => Int = (x: Int) => x + x

scala> Set(doubleByMult).contains(doubleByPlus)
res0: Boolean = false
```

This is just impossible, in the general case, to know if two functions compute the same thing. I didn't just say we don't know how to do it. It is actually proven that this is impossible. Like no one can and no one could for ever! Have a look at this [List of undecidable problems](https://en.wikipedia.org/wiki/List_of_undecidable_problems) for more information on the subject. Using extensional equality (the one where `f == g` *if and only* `f(x) == g(x)` for all `x`), there is just no implementation of `Set[Int => Int]`. But if `Set` was a functor, it would be trivial using `map` to get a `Set[Int => Int]`:

```scala
Set[Boolean](true, false).map {
  case true  => doubleByMult
  case false => doubleByPlus
}: Set[Int => Int]
```

The conclusion is `Set` is not a functor ... in the usual (i.e. *Scal*) category. But it is in for some categories. Let's define a functor as:

```scala
trait GenFunctor[predicate[_],F[_]] {
  def map[A,B](fa: F[A])(f: A => B)(implicit proofA: predicate[A], proofB: predicate[B]): F[B]
}
```

Then `Set` is a functor with `Ordering` as `predicate`:

```scala
object SetInstance extends GenFunctor[Ordering, Set] {
  def map[A,B](fa: Set[A])(f: A => B)(implicit orderingA: Ordering[A], orderingB: Ordering[B]): Set[B] =  {
    val set = TreeSet.newBuilder(orderingB)
    for (a <- fa) set += f(a)
    set.result
  }
}
```

Even `String` can be a functor!!!

```scala
sealed abstract class IsItChar[A]
implicit final case object YesItIsChar extends IsItChar[Char]

type StringK[A] = String

object StringInstance extends GenFunctor[IsItChar, StringK] {
  def map[A,B](fa: String)(f: A => B)(implicit proofA: IsItChar[A], proofB: IsItChar[B]): String =
    (proofA, proofB) match {
      case (YesItIsChar, YesItIsChar) => // A and B are both Char!
        fa.toList.map(f).mkString
    }
}
```

*GADTs* are an example of *Bushnell's law*. As you can see, they are easy to learn but can be used in very tricky situations which makes them hard to master. They are clearly very helpful in many situations but it seems they are still unfortunately very little used. *Haskell* supports them very well! *Scala*'s support is actually very good but not as good as *Haskell*. *Scala 3* will probably support them as well as *Haslell* since *Dotty*'s support is excellent. The only two other mainstream languages i know supporting them are *OCaml* and *Haxe*. Even if those two have a very good support, their lack of *Higer-Kinded types* forbids the most interesting uses.

As you probably know, it is possible to define a `fold` functor for every *Algebraic Data Type*. It is also possible to define `fold` functions for every *GADT*. As an exercise, try to define fold functions for the following *GADTs*:

- This *GADT* encode the equality between two types `A` and `B`:

    ```scala
    sealed abstract class Eq[A,B]
    final case class Refl[A]() extends Eq[A,A]
    ```

- This *GADT* represent an unknown type for which we have an instance of a type-class:

    ```scala
    sealed abstract class Ex[TypeClass[_]]
    final case class MakeEx[TypeClass[_],A](value:A, instance: TypeClass[A]) extends Ex[TypeClass]
    ```

You'll find how to define such `fold` functions [here]({{< ref "/posts/folds.md" >}}). Have fun and spread the love of *GADTs* everywhere :)