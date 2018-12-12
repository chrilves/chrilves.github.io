---
title: "Proving primality in Scala"
date: 2018-12-10T11:42:39+01:00
keywords: ["GADT", "Proof"] 
draft: true
---

Today we will explore the [Curry–Howard correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence). Our mission
is writing, in [Scala](https://www.scala-lang.org/)'s *type system*, the property
on natural number of being [prime](https://en.wikipedia.org/wiki/Prime_number). Wikipedia defines it by:

> A natural number (1, 2, 3, 4, 5, 6, etc.) is called a **prime number** (or a **prime**) if it is greater than 1 and cannot be written as a product of two natural numbers that are both smaller than it.

An equivalent way to put it is:

> A natural number is prime if it is **greater than 1 and cannot be divided by any natural number greater than 1 but smaller than it**.

These definitions are equivalent as, by definition, any natural number *n* is divisible by *k* if and only if it can be written *n = k × p* for some natural number *p*.

Writing a program whose execution checks whether a number is prime is easy. But we are not interested in executing programs, only compiling them! We want the **compiler to verify**
that a number is indeed prime. At that point, you may wonder how it is even possible to use the compiler to "prove" something about numbers. That's exactly the point of the
[Curry–Howard correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence)

# The Challenge

You can write any **positive integer** in the input bow below:
{{< primeui >}}

- Please write `3` in the input box above. A button shall appear
  letting you download `Prime3.scala`. Run it via

    ```sh
    scala Prime3.scala
    ```

    The file should compile and run flawlessly outputting `ForAllCons(NotDivRec(NotDivBasic(ListIsPositive(),LTTrivial()),AddPlus1(AddZero())),ForAllNil())`. Look into `Prime3.scala`,
    you should see a value `prime3: Prime[_3]` defined. The `main` method simply outputs this value.

- Now, write `4` in the input box. Download and run `Prime4.scala` via

    ```sh
    scala Prime4.scala
    ```

    The file should compile but execution should failed with the exception `scala.NotImplementedError: an implementation is missing`. Look into `Prime4.scala`,
    the value `prime4: Prime[_4]` is defined by `???`.

- Read `Prime4.scala` carefully, starting from the beginning, and **try to write a valid
  definition for** `val prime4: Prime[_4]`. Remember to **follow very scrupulously the rules
  stated in the first comment of `Prime4.scala`**.

  - DO NOT ALTER, IN ANY WAY, THE DEFINITION OF ANY TYPE IN THE FILE
  - DO NOT ADD SUB CLASSES/OBJECTS TO TYPES IN THE FILE
  - DO NOT USE NULL IN ANY WAY
  - ONLY USE THE GIVEN CASE OBJECTS AND CASE CLASSES IN THE FILE
  - THE GOAL IS TO PRODUCE A `val prime4: Prime[_4]`,
      NOT A `def prime4: Prime[_4]`,
      NOT A `lazy val prime4: Prime[_4]`!
  - YOUR CODE SHOULD TYPE-CHECK AND RUN PRINTING THE VALUE `prime4`

**Try to find valid values of type** `Prime[_N]` **when is not a prime number.**

# What the hell is going on ???

To encode properties over natural number, we need to start by encoding natural numbers.
To do so, we associate to any natural number a type:

- to 0 we associate the type `String`
- to 1 we associate the type `List[String]`
- to 2 we associate the type `List[List[String]]`
- to 3 we associate the type `List[List[List[String]]]`
- and so on:

    ```scala
    type _0 = String
    type _1 = List[_0] // List[String]
    type _2 = List[_1] // List[List[String]]
    type _3 = List[_2] // List[List[List[String]]]
    type _4 = List[_3] // List[List[List[List[String]]]]
    type _5 = List[_4] // List[List[List[List[List[String]]]]]
    ```

The next step is to define the type `Prime[N]` such that:

> There exists a **valid** of type `Prime[N]` **if and only if** `N` is (the type associated to) a **prime** number.

To each and every type in `PrimeN.scala` corresponds a property about natural numbers:

- 

# The "Proof"