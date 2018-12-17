---
title: "Episode 1 - Why Types?"
date: 2018-12-16T00:01:00+00:00
draft: false
type: "types"
episode: 1
episodeAsString: "first"
---

**Why using** types? **When** are they useful? **Why** we should **care** about types? **What** are types? These are the questions [this series of posts]({{< ref "/types" >}}) give some answers to. Let's start by realizing a fundamental fact: **types are everywhere**! Business models are all about types: users, shipments, orders, etc. Placing a user in the cart or shipping a client just makes no sense. **Types are specification**, discriminating what makes sense from what makes not.

## A real life example

They can be simple like the well known *strings*, *integers*, *booleans*, *lists*, *chars* and *floating-point numbers*. But these six are far from covering the whole picture. Take as an example, a business application about finding the best deal amount a list of prices expressed in various currencies. To do so it would need currency conversion rates. Which data structure would you use to store these rates? One euro (`EUR`) worths when i write these lines *1.13* dollars (`USD`) which i could represent as the triple `("EUR", 1.13 ,"USD")`. To store many rates, using a list is straightforward:

```scala
scala> val rates = List( ("EUR", 1.13, "USD"), ("GBP",  142.79, "JPY") )
rates: List[(String, Double, String)] = List((EUR,1.13,USD), (GBP,142.79,JPY))
```

Are you sure the type `(String, Double, String)` is a faithful representation for rates? Among all the possible values of this type is `("EUR", -3.0, "USD")` meaning when you give one euro, you have to *pay* three dollars more to get nothing. This is silly, currency trading don't work that way. Another problematic value is `("EUR", 0, "USD")`. What is the point in exchanging one euro for nothing? Using `Double` to encode rates is wrong as not all *floating point number* makes a valid rate, only those *strictly positive*! A faithful representation of rates is the type `r:Double{r > 0}` which is the types of value `r:Double` such that `r > 0`. Such types are called [refinement types](https://en.wikipedia.org/wiki/Refinement_type) and used heavily in [F*](https://www.fstar-lang.org/).

We're not done yet! Are you sure `String` is faithful representation of currencies? The empty string `""` is not a currency, neither are `"⛱"`, `"⛏"`, etc but they are all valid strings. Assuming `currencies:Set[String]` is the set of all valid currency names, the type of currencies would be `cur:String{currencies.contains(cur)}` if it was expressible in *Scala*. But it is in [F*](https://www.fstar-lang.org/):

```ocaml
open FStar.Set

val currencies: set string
let currencies = Set.as_set ["EUR"; "USD"; "JPY"]

type currency = cur:string{Set.mem cur currencies}
```

For the rest of this section, we will assume *Scala* has refinement types so that we can write:

```scala
type Rate = r:Double{r > 0}

val currencies: Set[String] =
  Set("EUR", "USD", "JPY")

type Currency = cur:String{currencies.contains(cur)}
```

We're still not done yet as the type `(Currency, Rate, Currency)` still does not faithfully encode a conversion rate: `("EUR", 2.0, "EUR")` is a value of this type but one euro can not worth two euros, this would not make sense! We could define the type `Rate` such that the only possible value is `1.0` when there is the same currency on both sides, but for simplicity's sake and without loss of generality, we will just forbid converting into the same currency. Once again, in a version of *Scala* with *refinement and dependent types*, conversion rates would be:

```scala
final case class ConvRate(
  from: Currency,
  rate: Rate,
  to:Currency{from != to}
)
```

We're almost there! Now every value of `ConvRate` makes sense from a business perspective: currencies `from` and `to` are valid distinct currencies and `rate` is a strictly positive exchange rate. Does the type `List[ConvRate]` faithfully represents a valid set of conversion rates? Values like `List(ConvRate("EUR", 2.0, "USD"), ConvRate("EUR", 1.13, "USB"))`, where there are several distinct rates for the same conversion, do not make sense. Likewise, there is a relation between a conversion rate and its opposite direction. If one euros worth `1.13` dollars, then one dollar worths `0.88` euros. Values like `List(ConvRate("EUR", 1.13, "USD"), ConvRate("USD", 2.0, "EUR"))` do not respect this rule, so are invalid business values. To be sure values do respect these **business rules** we can use the data structure [Map](https://www.scala-lang.org/api/2.12.4/scala/collection/immutable/Map.html) and only allow one side of the conversion (`from < to`) in the type `ConversionRates`:

```scala
val currencies: Set[String] =
  Set("EUR", "USD", "JPY")

final case class Conversion(
  from: String{currencies.contains(from)},
  to: String{currencies.contains(to) && from < to }
)

type ConversionRates = Map[Conversion, rate:Double{rate > 0}]
```

The type `ConversionRates` is certainly more complex than `List[(String, Double, String)]` but we gained several major benefits:

- every piece of code taking `ConversionRates` **values** as input is sure they **respect business rules**.
- every piece of code outputting `ConversionRates` values is **guaranteed to respect business rules**.
- **division by zero is impossible!**
  - no need to check!
  - no risk of exceptions!
  - no need to write tests!
  - no need to test the tests!

## Conclusion

You may think you are such an excellent developer than all of this is useless because you would never define a negative rate or an empty string currency. But how could you be sure that the previous developers on the project were as perfect as you? Have you written the thousands of tests required to reach the same level of confidence types provide? Will you be happy refactoring those thousands of test every time the specification change? Are you sure you know perfectly every one of the millions ones of the project? Even the one you never had to work with?

If we were as good as we think we are, there would not be null pointer exceptions, segfaults, exploits, undefined-is-not-a-function, `java.util.NoSuchElementException: None.get` and all the other so common bugs and vulnerabilities we see far too often! Software are complex beasts, made usually by many people, some who leave, some who join. Relying of people being perfect (perfect knowledge, perfect understanding, perfect execution) is not realistic.

You can see type systems as powerful linters whose job is to check your code. Types get more and more complex as business rules do from simple properties to [heavy mathematical theorems](https://homotopytypetheory.org/book/).

**Types are specification**, so in an ideal world a program well-typed would be, by definition, a program without bugs. If you still find bugs in your application, it either means you need to tighten your types to fit the specification or your language's type-system is not expressive enough to encode the specification.

## [Next Episode]({{< ref "types/2-enums.md" >}})

In the [next episode]({{< ref "types/2-enums.md" >}}), we will see enumerations like you probably have never seen them before.