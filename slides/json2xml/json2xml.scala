import scala.xml._

object Presentation {
  import play.api.libs.json._

  def json2xml(json: JsValue, rootLabel: String): Elem = {
    def mkRoot(jsType: String, children: Node*): Elem =
      Elem(null,
          rootLabel,
          new UnprefixedAttribute("type", jsType, scala.xml.Null),
          TopScope,
          true,
          children*
          )

    json match {
      case JsNull => mkRoot("null")
      case JsString(s) => mkRoot("string", Text(s))
      case JsNumber(n) => mkRoot("number", Text(n.toString))
      case JsBoolean(b) => mkRoot("boolean", Text(b.toString))
      case JsArray(l) => mkRoot("array", l.map(json2xml(_, s"${rootLabel}Item")):_*)
      case JsObject(m) => mkRoot("object", m.toList.map { case (k,v) => json2xml(v, k) }*)
    }
  }
}

object PlayLike {
  sealed abstract class JsValue
  final case object JsNull extends JsValue
  final case class JsNumber(value: BigDecimal) extends JsValue
  final case class JsBoolean(value: Boolean) extends JsValue
  final case class JsString(value: String) extends JsValue
  final case class JsArray(value: List[JsValue]) extends JsValue
  final case class JsObject(value: Map[String, JsValue]) extends JsValue
}

object MainPoint {
  import cats.{Functor, Applicative, Monad, Traverse}
  import cats.instances.list._
  import cats.instances.option._
  import cats.syntax.functor._
  import cats.syntax.traverse._
  import cats.syntax.flatMap._

  sealed abstract class Atomic
  final case object Null extends Atomic
  final case class Bool(value: Boolean) extends Atomic
  final case class Number(value: BigDecimal) extends Atomic
  final case class Str(value: String) extends Atomic

  sealed abstract class JsValue
  final case class JsAtom(value: Atomic) extends JsValue
  final case class JsArray(value: List[JsValue]) extends JsValue
  final case class JsObject(value: Map[String, JsValue]) extends JsValue

  //def array(list: List[JsValue]): JsValue = JsArray(list)
  //def obj(fields: Map[String, JsValue]): JsValue = JsObject(fields)


  sealed abstract class JsLike[+R]
  final case class Atom(value: Atomic) extends JsLike[Nothing]
  final case class Arr[+R](value: List[R]) extends JsLike[R]
  final case class Obj[+R](value: Map[String, R]) extends JsLike[R]

  // JsValue

  val jsLike2JsValue: (JsLike[JsValue] => JsValue) = {
    case Atom(a)         => JsAtom(a)
    case Arr(a)          => JsArray(a)
    case Obj(m)          => JsObject(m)
  }

  val jsValue2JsLike: (JsValue => JsLike[JsValue]) = {
    case JsAtom(a)    => Atom(a)
    case JsArray(a)   => Arr(a.toList)
    case JsObject(m)  => Obj[JsValue](m)
  }

  // XML

  val jsLike2xml: JsLike[String => Elem] => (String => Elem) = {
    def mkRoot(jsType: String, children: Node*): String => Elem =
      (someRootTag: String) =>
        Elem(null,
            someRootTag,
            new UnprefixedAttribute("type", jsType, scala.xml.Null),
            TopScope,
            true,
            children*
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
            mkRoot("array", a.map(_(s"${root}Item"))*)(root)
          }

        case Obj(m) =>
          mkRoot("object", m.toList.map { case (k, v) => v(k) }*)
      }
  }


  import cats.data.ReaderT
  type TagOpt[A] = ReaderT[Option, String, A]

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
                case "true" => Some(Atom(Bool(true)))
                case "false" => Some(Atom(Bool(false)))
                case _ => None
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
                    case _ => Nil
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

  trait Direct {
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

  def hylo[A,B, F[_]: Functor](decompose: A => F[A], recompose: F[B] => B): A => B = {
    def convert(a: A): B =
      recompose(decompose(a).map(convert))
    
    convert _
  }
    
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


  def json2xml(json: JsValue): String => Elem =
    hylo(jsValue2JsLike, jsLike2xml).apply(json)

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

  def hyloish[A,B, F[_]: Traverse, M[_]: Monad](decompose: A => M[F[A]], recompose: F[B] => B): A => M[B] = {
    def convert(a: A): M[B] =
      for {
      fa <- decompose(a)
      fb <- fa.traverse(convert)
    } yield recompose(fb)

    convert _
  }

  def xml2json(f: String => Elem): TagOpt[JsValue] =
    hyloish(xml2JsLike, jsLike2JsValue).apply(f)

  val json2xmlBetter: ((String, JsValue)) => Elem =
    (jsonPlusTag: (String, JsValue)) => json2xml(jsonPlusTag._2)(jsonPlusTag._1)

  val xml2jsonBetter: Elem => TagOpt[(String, JsValue)] =
    (e: Elem) => xml2json((s: String) => e.copy(label = s)).map(e.label -> _)
}