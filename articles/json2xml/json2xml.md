# JSON to XML: the probably a tiny bit over engineered way

**[The complete code of the article](https://github.com/chrilves/chrilves.github.io/blob/master/articles/json2xml/json2xml.scala). You need  [Cats](https://typelevel.org/cats/) and [Play-Json](https://github.com/playframework/play-json) in order to run it.**

It happens regularly in software development that we have to connect systems speaking different languages. JSON is nowadays ubiquitous in service communication, especially in web development but XML still has its fair amount of bastions. Imagine you need to pass information provided by a JSON API through an XML layer, you need a converter.

## The easy way

This translation is actually pretty trivial, it takes essentially 6 lines of simple pattern-matching code in [Scala](https://www.scala-lang.org/):

```scala
import play.api.libs.json._
import scala.xml._

def json2xml(json: JsValue, rootLabel: String): Elem = {
  // XML node creation helper
  def mkElem(jsType: String, children: Node*): Elem =
    Elem(null, rootLabel,
         new UnprefixedAttribute("type", jsType, scala.xml.Null),
         TopScope, true, children: _*
        )

  // The real translation
  json match {
    case JsNull =>
      mkElem("null")

    case JsString(s) =>
      mkElem("string", PCData(s))

    case JsNumber(n) =>
      mkElem("number", Text(n.toString))

    case JsBoolean(b) =>
      mkElem("boolean", Text(b.toString))

    case JsArray(l) =>
      mkElem("array", l.map(json2xml(_, s"${rootLabel}Item")):_*)

    case JsObject(m) =>
      mkElem("object", m.toList.map { case (k,v) => json2xml(v, k) }: _*)
  }
}
```

The trickiest part of this example is figuring out how to build XML nodes in *Scala*. It translates the following JSON:

```json
[
  { "title": "2001 : A Space Odyssey",
    "release":
      { "day": 27,
        "month": 9,
        "year": 1968
      },
    "genres" : [ "Science fiction" ],
    "actors": [
      { "lastName": "Dullea",
        "firstName": "Keir",
        "role": "Dr. David Bowman"
      }
    ],
    "directors": [
      { "lastName": "Kubrick",
        "firstName": "Stanley"
      }
    ]
  }
]
```

into

```xml
<films type="array">
  <filmsItem type="object">
    <title type="string"><![CDATA[2001 : A Space Odyssey]]></title>
    <release type="object">
      <day type="number">27</day>
      <month type="number">9</month>
      <year type="number">1968</year>
    </release>
    <genres type="array">
      <genresItem type="string"><![CDATA[Science fiction]]></genresItem>
    </genres>
    <actors type="array">
      <actorsItem type="object">
        <lastName type="string"><![CDATA[Dullea]]></lastName>
        <firstName type="string"><![CDATA[Keir]]></firstName>
        <role type="string"><![CDATA[Dr. David Bowman]]></role>
      </actorsItem>
    </actors>
    <directors type="array">
      <directorsItem type="object">
        <lastName type="string"><![CDATA[Kubrick]]></lastName>
        <firstName type="string"><![CDATA[Stanley]]></firstName>
      </directorsItem>
    </directors>
  </filmsItem>
</films>
```

Note that, unlike JSON, XML have no notion of booleans, number or null, so we add type information as attribute on each node. This has the benefit of enabling us to convert such XML back to their former JSON form. Also note that, we need *CDATA* sections to preserve spaces.

Problem solved? Yes! But we can go much much further on this subject...

# The Rocket Science way

There much more thing to say about this example, first let's expose some properties of JSON values.

## Inviting (Co)Algebras to the Party

JSON values can be modelled with an [Algebraic Data Type](https://en.wikipedia.org/wiki/Algebraic_data_type) or *ADT* for short. [Play-Json](https://github.com/playframework/play-json) represents them by the type `JsValue`:

```scala
sealed abstract class JsValue
final case object JsNull extends JsValue
final case class JsNumber(value: BigDecimal) extends JsValue
final case class JsBoolean(value: Boolean) extends JsValue
final case class JsString(value: String) extends JsValue
final case class JsArray(value: List[JsValue]) extends JsValue
final case class JsObject(value: Map[String, JsValue]) extends JsValue
```

But in order to simplify the presentation, we will use slightly different, but **equivalent**, definition of JSON values:

```scala
sealed abstract class Atomic
final case object Null extends Atomic
final case class Bool(value: Boolean) extends Atomic
final case class Number(value: BigDecimal) extends Atomic
final case class Str(value: String) extends Atomic

sealed abstract class JsValue
final case class JsAtom(value: Atomic) extends JsValue
final case class JsArray(value: List[JsValue]) extends JsValue
final case class JsObject(value: Map[String, JsValue]) extends JsValue
```

Like in any Algebraic Data Type, the **constructors** of `JsValues` can be seen as operations on it. `JsAtom` informs us that every number, boolean, string and `null` give rise to a distinct JSON value. `JsArray` and `JsObject` tells us that each (qualified) list of JSON values forms a distinct JSON value itself. Considering that JSON values are defined in terms of these operations, and that we want to translate JSON into XML, it would make sense to define them on XML as well. First, let's explicit these operations:

```scala
sealed abstract class JsLike[+R]
final case class Atom(value: Atomic) extends JsLike[Nothing]
final case class Arr[+R](value: List[R]) extends JsLike[R]
final case class Obj[+R](value: Map[String, R]) extends JsLike[R]
```

The interesting point here is we can translate back and forth between `JsValue` and `JsLike[JsValue]`. These translations are even the inverse of each other, meaning both types are totally equivalent!

```scala
val jsLike2JsValue: JsLike[JsValue] => JsValue = {
  case Atom(a)         => JsAtom(a)
  case Arr(a)          => JsArray(a)
  case Obj(m)          => JsObject(m)
}

val jsValue2JsLike: JsValue => JsLike[JsValue] = {
  case JsAtom(a)    => Atom(a)
  case JsArray(a)   => Arr(a.toList)
  case JsObject(m)  => Obj(m)
}
```

`jsLike2JsValue` is called a `JsLike`-Algebra because it has the form `JsLike[X] => X`. It means `jsLike2JsValue` is a way "compute" `JsLike` operation, i.e. it composes values to form new ones. On the opposite, `jsValue2JsLike` is called a `JsLike`-CoAlgebra because it has the form `X => JsLike[X]`. It is a way to expose how a value is built, i.e. it deconstructs values to expose their structure.

Can we find such functions for XML values? We are looking for two functions:

```scala
val jsLike2Elem: JsLike[Elem] => Elem = ???
val elem2JsLike: Elem => JsLike[Elem] = ???
```

It would certainly be nice, but unfortunately this is not that simple! `5`, `true` and `null` are valid JSON values, So `jsLike2Elem(Atom(Number(5)))`, `jsLike2Elem(Atom(Bool(true)))` and `jsLike2Elem(Atom(Null)))` should be valid XML value! But what should be the root tag of the resulting elements? How to translate `5` into a valid XML? We know that it would have the form:

```xml
<someRootTag type="number">5</someRootTag>
```

But what `someRootTag` should be? We could pick an arbitrary one, but it would break composability (try it, you'll see!). There's no escape, all XML values need tags but not every JSON value have some! The situation suggest JSON values are closer to "XML values with unknown root tags" `<X type="number">5</X>` where `X` as the unknown, i.e. the functional space `String => Elem`:

```scala
val _5: String => Elem =
  (someRootTag: String) => <someRootTag type="number">5</someRootTag>
```

Do you think we can define meaningful functions?

```scala
val jsLike2xml: JsLike[Elem => String] => (String => Elem) = ???
val xml2JsLike: (String => Elem) => JsLike[String => Elem] = ???
```

Yes we can ... partialy. We can define `jsLike2xml`:

```scala
val jsLike2xml: JsLike[String => Elem] => (String => Elem) = {
  def mkRoot(jsType: String, children: Node*): String => Elem =
    (someRootTag: String) =>
      Elem(null,
           someRootTag,
           new UnprefixedAttribute("type", jsType, scala.xml.Null),
           TopScope,
           true,
           children: _*
          )

  (j: JsLike[String => Elem]) =>
    j match {
      case Atom(Null)   =>
        mkRoot("null")

      case Atom(Str(s)) =>
        mkRoot("string", PCData(s))

      case Atom(Bool(b)) =>
        mkRoot("boolean", Text(b.toString))

      case Atom(Number(n)) =>
        mkRoot("number", Text(n.toString))
        
      case Arr(a) =>
        (root: String) => {
          mkRoot("array", a.map(_(s"${root}Item")): _*)(root)
        }

      case Obj(m) =>
        mkRoot("object", m.toList.map { case (k, v) => v(k) }: _*)
    }
}
```

but for `xml2JsLike`, we're facing two not-that-small issues:

- First, unlike`jsValue2JsLike`, we can not pattern-match on functions. We have no sane way to know that

  ```xml
  (someRootTag: String) => <someRootTag type="number">5</someRootTag>
  ```

  is built from `Atom(Number(5))`.

- Even if we could pattern-match on functions, `jsLike2xml` is not surjective, i.e. not every XML element is the result of `jsLike2xml(f)` for some `f`. To deal with invalid input, the return type of `xml2JsLike` can not be `JsLike[String => Elem]` but `F[JsLike[String => Elem]]` for some functor `F` able to deal with errors like `Option`, `Either`, etc. For simplicity's sake, let's consider `F` to be `Option`.


Let's once again take a step back. We want to decompose a function `(f: String => Elem)` into an `Option[JsLike[String => Elem]]` without pattern-matching it. The only reasonable thing we can do with functions is pass them some arguments:

```scala
def xml2JsLike(f: (String => Elem)): String => Option[JsLike[Elem => String]] =
  (someRootTag: String) => ... f(someRootTag) ...
```

The type `String => Option[A]` is actually a monad, known as a `ReaderT[Option, String, A]`. Which makes `xml2JsLike` a monadic coalgebra. Let's give it a name:

```scala
import cats.data.ReaderT

type TagOpt[A] = ReaderT[Option, String, A]
```

As an exercise try to implement `xml2JsLike`. *To that end, it may be useful to notice that `JsLike` is a `Traverse`, i.e. that an instance of `Traverse[JsLike]` can be defined. Such an instance defines a function:
```scala
def `traverse[G[_]: Applicative, A, B](ja: JsLike[A])(f: A => G[B]): G[JsLike[B]]`
```

To summarize this part, we have these four functions:

```scala
val jsLike2JsValue: JsLike[JsValue]    => JsValue
val jsValue2JsLike: JsValue            => JsLike[JsValue]

val jsLike2xml: JsLike[String => Elem] => (String => Elem)
val xml2JsLike: (String => Elem)       => TagOpt[JsLike[String => Elem]]
```

Now we want to convert `JsValue` from/into `String => Elem`.

### Converting back and forth

Now that we know how to compose and decompose both JSON and XML values. How do we write converters? For simplify's sake, let's be a bit more abstract. Let `A` and `B` be to types (like `JsValue` and `String => Elem`) and `F[_]` a type constructor (like `JsLike`) that have the nice property of being a functor (i.e. it has function `map: F[A] => (A => B) => F[B]`). In addition, let `decomposeA: A => F[A]` and `recomposeB: F[B] => B` (like `jsValue2JsLike` and `jsLike2xml`). We want a function `convert: A => B`:

```scala
trait Direct {
  import cats.Functor
  import cats.syntax.functor._

  type A
  type B
  type F[_]

  implicit val fHasMap: Functor[F]

  val decomposeA: A    => F[A]
  val recomposeB: F[B] => B

  final def convert(a: A): B = {
    val fa: F[A] = decomposeA(a)
    val fb: F[B] = fa.map(convert)
    recomposeB(fb): B
  }
}
```

Or in a more compact way:

```scala
def hylo[A,B, F[_]: Functor](decompose: A => F[A], recompose: F[B] => B): A => B = {
  def convert(a: A): B =
    recompose(decompose(a).map(convert))

  convert _
}
```

And voila, a converter in just 1 lines of code:

```scala
def json2xml(json: JsValue): String => Elem =
  hylo(jsValue2JsLike, jsLike2xml).apply(json)
```

The way back is only a bit more involving. This time we require `F` to be `Traverse` and the function `decomposeA` to be of type `A => M[F[A]]` for some monad `M`:

```scala
trait WayBack {
  type A
  type B

  type F[_]
  implicit val fHasTraverse: Traverse[F]

  type M[_]
  implicit val mIsAMonad: Monad[M]

  val decomposeA: A    => M[F[A]]
  val recomposeB: F[B] => B

  final def convert(a: A): M[B] =
    for {
      fa <- decomposeA(a)
      fb <- fa.traverse(convert)
    } yield recomposeB(fb)
}
```

Again, in a more compact way:

```scala
def hyloish[A,B, F[_]: Traverse, M[_]: Monad](decompose: A => M[F[A]], recompose: F[B] => B): A => M[B] = {
  def convert(a: A): M[B] =
    for {
    fa <- decompose(a)
    fb <- fa.traverse(convert)
  } yield recompose(fb)

  convert _
}
```

which gives the way back as the oneliner:

```scala
def xml2json(f: String => Elem): TagOpt[JsValue] =
    hyloish(xml2JsLike, jsLike2JsValue).apply(f)
```

Reorganizing a bit, it leads to the two conversion functions between `(String, JsValue)` and `Elem`:

```scala
val json2xmlBetter: ((String, JsValue)) => Elem =
  (jsonPlusTag: (String, JsValue)) => json2xml(jsonPlusTag._2)(jsonPlusTag._1)

val xml2jsonBetter: Elem => TagOpt[(String, JsValue)] =
  (e: Elem) => xml2json((s: String) => e.copy(label = s)).map(e.label -> _)
```

## What's the point?

Apart from being so much more complicated that the trivial approach, is there some benefits? Actually yes.

- Firstly, given `n` formats, there are `n²` converters. Writing and testing `n²` functions is a lot of tedious and error-prone work. But if you find some common operations `F[_]`, you only need `2n` functions (one `X => F[X]` and one `F[X] => X` for each format `X`) to achieve the same goal. Furthermore, each of those functions will be easier to test, which is not to neglect.
- Secondly, algebras (functions `X => F[X]`) and coalgebras (functions `F[X] => X`) operate one level at a time. They enable to treat format `X` as if it was an algebraic data type over operations `F`. Pattern-matching is such a nice feature!
- Thirdly, you can write generic functions taking any type `X` for which you can provide functions `X => F[X]` and `F[X] => X`. These functions also have higher chances of being correct because there is less space for unexpected behaviour.

## Solution to exercises

### `JsLike` instance for `Traverse`

```scala
implicit val jsLikeInstances: Traverse[JsLike] =
  new Traverse[JsLike] {
    import cats.Eval

    def traverse[G[_], A, B](fa: JsLike[A])(f: A => G[B])(
        implicit G: Applicative[G]): G[JsLike[B]] =
      fa match {
        case Atom(a) => G.point(Atom(a))
        case Arr(a)  => a.traverse[G, B](f).map(Arr(_))
        case Obj(m) =>
          m.toList
            .traverse[G, (String, B)] { case (s, a) => f(a).map(s -> _) }
            .map(i => Obj(i.toMap))
      }

    def foldLeft[A, B](fa: JsLike[A], b: B)(f: (B, A) => B): B =
      fa match {
        case Atom(_) => b
        case Arr(a)  => a.foldLeft(b)(f)
        case Obj(m)  => m.values.foldLeft(b)(f)
      }

    def foldRight[A, B](fa: JsLike[A], lb: Eval[B])(
        f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa match {
        case Atom(_) => lb
        case Arr(a)  => a.foldRight(lb)(f)
        case Obj(m)  => m.values.foldRight(lb)(f)
      }
  }
```

### `xml2JsLike`

```scala
def xml2JsLike(f: String => Elem): TagOpt[JsLike[String => Elem]] =
  ReaderT[Option, String, JsLike[String => Elem]] { (s: String) =>
    val elem: Elem = f(s)

    elem
      .attributes
      .asAttrMap
      .get("type")
      .flatMap[JsLike[String => Elem]] {
        case "null" =>
          Some(Atom(Null))

        case "boolean" =>
          elem.text match {
            case "true"  => Some(Atom(Bool(true)))
            case "false" => Some(Atom(Bool(false)))
            case _       => None
          }

        case "number" =>
          import scala.util.Try

          Try(BigDecimal(elem.text))
            .toOption
            .map(n => Atom(Number(n)))

        case "string" =>
          Some(Atom(Str(elem.text)))

        case "array" =>
          Some(Arr(
            elem
              .child
              .toList
              .flatMap {
                case e: Elem => List((s: String) => e.copy(label = s))
                case _       => Nil
              }
          ))

        case "object" =>
          Some(Obj(
            elem
              .child
              .toList
              .flatMap {
                case e: Elem => List(e.label -> ((s: String) => e.copy(label = s)))
                case _       => Nil
              }.toMap
          ))

        case _ =>
          None
    }
  }
```
