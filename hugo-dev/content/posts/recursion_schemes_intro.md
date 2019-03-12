---
title: "Recursion Schemes: the high-school introduction"
date: 2019-02-25T17:42:18+01:00
draft: true
---

## Factorial

`fact(n) = 1 * 2 * 3 * ... * n`
`sum(n)  = 1 + 2 + 3 + ... + n`


```scala
def fact(n: Int): Int =
  if (n == 0) 1
  else {
    val r = fact(n-1)
    n * r
  }

def sum(n: Int): Int =
  if (n == 0) 0
  else {
    val r = sum(n-1)
    n + r
  }
```


```scala
val baseCase: Int = 1
def recCase(n: Int, r: Int): Int = n * r

def fact(n: Int): Int =
  if (n == 0) baseCase
  else {
    val r = fact(n-1)
    recCase(n, r)
  }
```

```scala
val baseCase: Int = 0
def recCase(n: Int, r: Int): Int = n + r

def sum(n: Int): Int =
  if (n == 0) baseCase
  else {
    val r = sum(n-1)
    recCase(n, r)
  }
```

```scala
def scheme(baseCase: Int, recCase: (Int, Int) => Int)(n: Int): Int =
  if (n == 0) baseCase
  else {
    val r = scheme(n-1)
    recCase(n, r)
  }

def fact: Int => Int = scheme(1, (n: Int, r:Int) => n * r)
def sum : Int => Int = scheme(0, (n: Int, r:Int) => n + r)
```

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

def fact: Int => Int = scheme(1, (n: Int, r:Int) => n * r)
def sum : Int => Int = scheme(0, (n: Int, r:Int) => n + r)
```

## Fibonacci


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

```scala
val baseCase0: Int = 1
val baseCase1: Int = 1
def recCase(r1: Int, r2: Int): Int = r1 + r2

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

```scala
def scheme(baseCase0: Int, baseCase1: Int, recCase: (Int, Int) => Int)(n: Int): Int =
  n match {
    case 0 => baseCase0
    case 1 => baseCase1
    case n =>
      val r1 = fib(n-1)
      val r2 = fib(n-2)
      recCase(r1, r2)
  }

def fib: Int => Int = scheme(1, 1, (r1: Int, r2: Int) => r1 + r2)
def abc: Int => Int = scheme(3, 2, (r1: Int, r2: Int) => r1 * r2)
```

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

def fib: Int => Int = scheme(1, 1, (r1: Int, r2: Int) => r1 + r2)
def abc: Int => Int = scheme(3, 2, (r1: Int, r2: Int) => r1 * r2)
```

## Not Only Int

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

def fact: Int => Int = scheme[Int](1, (n: Int, r:Int) => n * r)

def list: Int => List[Int] =
  scheme[List[Int]](Nil, (n: Int, r: List[Int]) => n :: r)
```


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

def fib: Int => Int = scheme(1, 1, (r1: Int, r2: Int) => r1 + r2)

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