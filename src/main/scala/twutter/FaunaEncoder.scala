package twutter

import cats.implicits.*
import com.faunadb.client.query.Expr
import com.faunadb.client.query.Language.{Obj, Arr}
import com.faunadb.client.types.{Codec, Value}

import java.time.{Instant, LocalDate}
import scala.compiletime.*
import scala.deriving.*
import scala.jdk.CollectionConverters.*


trait FaunaEncoder[A]:
  extension(value: A) def encode: Expr

object FaunaEncoder:
  private inline def summonEncoders[T <: Tuple]: List[FaunaEncoder[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[FaunaEncoder[t]] :: summonEncoders[ts]

  private inline def summonLabels[L <: Tuple]: List[String] =
    inline erasedValue[L] match
      case _ : EmptyTuple => Nil
      case _ : (name *: names) => constValue[name].toString :: summonLabels[names]

  inline given derived[A <: Product](using m: Mirror.ProductOf[A]): FaunaEncoder[A] =
    new FaunaEncoder[A]:
      extension(value: A) def encode: Expr = {
        val encoders = summonEncoders[m.MirroredElemTypes]
        val names = summonLabels[m.MirroredElemLabels]
        val values = value.productIterator
        val pairs = encoders zip names zip values
        val obj = pairs.foldLeft(Map.empty[String, Expr]) { case (obj, ((encoder, name), value)) =>
          obj + (name -> encoder.asInstanceOf[FaunaEncoder[Any]].encode(value))
        }

        Obj(obj.asJava)
      }


  private def encodeWith[A](value: A, codec: Codec[A]): Expr =
    codec.encode(value).orNull()

  given FaunaEncoder[String] with
    extension(value: String) def encode: Expr = encodeWith(value, Codec.STRING)

  given FaunaEncoder[Instant] with
    extension(value: Instant) def encode: Expr = encodeWith(value, Codec.TIME)

  given FaunaEncoder[Expr] with
    extension(value: Expr) def encode: Expr = value

  given FaunaEncoder[LocalDate] with
    extension(value: LocalDate) def encode: Expr =encodeWith(value, Codec.DATE)

  given FaunaEncoder[Boolean] with
    extension(value: Boolean) def encode: Expr =encodeWith(new java.lang.Boolean(value), Codec.BOOLEAN)

  given FaunaEncoder[Double] with
    extension(value: Double) def encode: Expr =encodeWith(new java.lang.Double(value), Codec.DOUBLE)

  given FaunaEncoder[Long] with
    extension(value: Long) def encode: Expr =encodeWith(new java.lang.Long(value), Codec.LONG)

  given FaunaEncoder[Float] with
    extension(value: Float) def encode: Expr =encodeWith(new java.lang.Float(value), Codec.FLOAT)

  given FaunaEncoder[Int] with
    extension(value: Int) def encode: Expr =encodeWith(new java.lang.Integer(value), Codec.INTEGER)

  given [A](using d: FaunaEncoder[A]): FaunaEncoder[List[A]] with
    extension(value: List[A]) def encode: Expr =Arr(value.map(d.encode).asJava)


