object FP  {
    trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
    
    final def map[A,B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)((a:A) => pure(f(a)))

    final def ap[A,B](fa: F[A])(f: F[A => B]): F[B] =
      flatMap(fa) { (a:A) =>
        map(f) { (g: A => B) =>
          g(a)
        }
      }
  }

  implicit final class MonadOps[F[_],A](val self: F[A])(implicit F: Monad[F]) {
    def map[B](f: A => B): F[B] = F.map(self)(f)
    def flatMap[B](f: A => F[B]): F[B] = F.flatMap(self)(f)
    def ap[B](f: F[A => B]): F[B] = F.ap(self)(f)
  }

  implicit final class MonadOpsFun[F[_],A,B](val self: F[A => B])(implicit F: Monad[F]) {
    def arg(fa : F[A]): F[B] = F.ap(fa)(self)
  }

  implicit final class ListOps[A](val self: List[A]) {
    def traverse[G[_], B](f: A => G[B])(implicit G : Monad[G]): G[List[B]] =
      self match {
        case hd :: tl =>
          for {
            valHd <- f(hd)
            valTl <- tl.traverse(f)
          } yield valHd :: valTl
        case _ =>
          G.pure(Nil)
      }
  }
}

object Json2Csv {
  sealed abstract class Atomic
  final case object Null extends Atomic
  final case class Bool(value: Boolean) extends Atomic
  final case class Number(value: BigDecimal) extends Atomic
  final case class Str(value: String) extends Atomic
     
  def atomic2str(a: Atomic): String =
    a match {
      case Null => ""
      case Bool(b) => b.toString
      case Number(n) => n.toString
      case Str(s) =>
        val espaced =
          s
            .replaceAll("""\\""", """\\\\""")
            .replaceAll("\"", """\\"""")
        "\"" + s + "\""
    }

  sealed abstract class JsValue
  final case class JsAtom(value: Atomic) extends JsValue
  final case class JsArray(value: List[JsValue]) extends JsValue
  final case class JsObject(value: Map[String, JsValue]) extends JsValue

  import FP._

  trait JsonOps[F[_]] extends Monad[F] {
    def emit(s: String): F[Unit]
    def choose[A](l : List[A]): F[A]
    def field[A](fld: String)(fa: F[A]): F[A]
  }

  def json2csv[F[_]](json: JsValue)(implicit F: JsonOps[F]): F[Unit] =
    json match {
      case JsAtom(a)   =>
        F.emit(atomic2str(a))
      
      case JsArray(l)  =>
        F.choose(l).flatMap { (js: JsValue) =>
          json2csv[F](js)
        }

      case JsObject(m) =>
        m.toList.traverse[F, Unit] { case (fld, js) =>
          F.field(fld)(json2csv[F](js))
        }.map(_ => ())
    }

  type Path = List[String]
  object Path {
    val empty: Path = Nil
  }

  final case class Json2CsvM[A](List[Map[Path, String]], List[A])

  implicit val json2CsvMInstances: JsonOps[Json2CsvM] =
    new JsonOps[Json2CsvM] {
      def pure[A](a:A): Json2CsvM[A] = Json2CsvM(Set.empty, a :: Nil)
      
      def flatMap[A,B](fa: Json2CsvM[A])(f: A => Json2CsvM[B]): Json2CsvM[B] =
        fa.values.foldLeft(Json2CsvM(fa.headers, Nil: List[B])) {
          case (Json2CsvM(headers, values), a) =>
            val fb: Json2CsvM[B] = f(a)
            Json2CsvM(headers ++ fb.headers, values ++ fb.values)
        }

      def choose[A](l: List[A]): Json2Csv.HeadersF[A] =
        HeadersF(Set.empty, l)
      
      def emit(s: String): Json2Csv.HeadersF[Unit] =
        HeadersF(Set(Path.empty), () :: Nil)
      
      def field[A](fld: String)(fa: Json2Csv.HeadersF[A]): Json2Csv.HeadersF[A] =
        HeadersF(fa.headers.map(fld :: _), fa.values)
    }

  
  val json1: JsValue = JsArray(List(
    JsObject(Map(
      "a" -> JsArray(List(
        JsAtom(Number(5)),
        JsAtom(Bool(true)),
        JsObject(Map(
          "d" -> JsAtom(Null),
          "e" -> JsAtom(Str("toto"))
        ))
      )),
      "b" -> JsAtom(Null),
      "c" -> JsArray(List(
        JsAtom(Number(5)),
        JsObject(Map(
          "d" -> JsAtom(Null),
          "e" -> JsAtom(Str("toto"))
        )),
        JsAtom(Bool(true))
      ))
    )),
    JsObject(Map(
      "c" -> JsArray(List(
        JsAtom(Number(5)),
        JsObject(Map(
          "e" -> JsAtom(Str("toto"))
        )),
        JsAtom(Bool(true))
      )),
      "a" -> JsArray(List(
        JsAtom(Number(5)),
        JsAtom(Bool(true)),
        JsObject(Map(
          "f" -> JsAtom(Str("toto")),
          "d" -> JsAtom(Null)
        ))
      ))
    ))
  ))
}

import FP._
import Json2Csv._