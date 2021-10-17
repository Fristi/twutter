
import com.faunadb.client.FaunaClient
import com.faunadb.client.query.Expr
import com.faunadb.client.types.{Codec, Value}
import zio.*
import zio.blocking.Blocking
import zio.stream.ZStream
import cats.implicits.*

import java.time.{Instant, LocalDate}
import scala.deriving.*
import scala.compiletime.*
import scala.jdk.CollectionConverters.*

enum DecodeError(error: String) extends Throwable(error):
  case FieldDoesNotExist(expr: Value, label: String) extends DecodeError(s"Field does not exists '$label' on $expr")
  case CannotDecode(expr: Value) extends DecodeError(s"Cannot decode $expr")

trait Decoder[A]:
  def decode(expr: Value): Either[DecodeError, A]


object Decoder:
  private inline def decoders[T <: Tuple]: List[Decoder[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[Decoder[t]] :: decoders[ts]

  private inline def labels[L <: Tuple]: List[String] =
    inline erasedValue[L] match
      case _ : EmptyTuple => Nil
      case _ : (name *: names) => constValue[name].toString :: labels[names]

  inline given [A <: Product](using m: Mirror.ProductOf[A]): Decoder[A] =
    new Decoder[A]:
      def decode(expr: Value): Either[DecodeError, A] = {
        val elems = decoders[m.MirroredElemTypes]
        val names = labels[m.MirroredElemLabels]
        val data = expr.at("data")

        elems.zip(names).traverse { case (decoder, name) =>
          val field: Value = data.at(name)

          field.orNull() match {
            case null => Left(DecodeError.FieldDoesNotExist(expr, name))
            case rest => decoder.decode(rest)
          }
        } map { res =>
          m.fromProduct(Tuple.fromArray(res.toArray)).asInstanceOf[A]
        }
      }

  private def decodeWith[A](expr: Value, codec: Codec[A]): Either[DecodeError, A] =
    Option(expr.to(codec).orNull()).fold(Left(DecodeError.CannotDecode(expr)))(Right.apply)

  given Decoder[String] with
    def decode(expr: Value): Either[DecodeError, String] = decodeWith(expr, Codec.STRING)

  given Decoder[Instant] with
    def decode(expr: Value): Either[DecodeError, Instant] = decodeWith(expr, Codec.TIME)

  given Decoder[LocalDate] with
    def decode(expr: Value): Either[DecodeError, LocalDate] = decodeWith(expr, Codec.DATE)

  given Decoder[Boolean] with
    def decode(expr: Value): Either[DecodeError, Boolean] = decodeWith(expr, Codec.BOOLEAN).map(_.booleanValue())

  given Decoder[Double] with
    def decode(expr: Value): Either[DecodeError, Double] = decodeWith(expr, Codec.DOUBLE).map(_.toDouble)

  given Decoder[Long] with
    def decode(expr: Value): Either[DecodeError, Long] = decodeWith(expr, Codec.LONG).map(_.toLong)

  given Decoder[Float] with
    def decode(expr: Value): Either[DecodeError, Float] = decodeWith(expr, Codec.FLOAT).map(_.toFloat)

  given Decoder[Int] with
    def decode(expr: Value): Either[DecodeError, Int] = decodeWith(expr, Codec.INTEGER).map(_.toInt)

  given [A](using d: Decoder[A]): Decoder[List[A]] with
    def decode(expr: Value): Either[DecodeError, List[A]] = decodeWith(expr, Codec.ARRAY).flatMap(_.asScala.toList.traverse(d.decode))



trait Fauna {
  def query[A](expr: Expr)(using d: Decoder[A]): Task[A]
}

object Fauna extends Accessible[Fauna] {

    def layer(secret: String): ZLayer[Blocking, Nothing, Has[Fauna]] =
      (for {
        blocking <- ZIO.service[Blocking.Service]
      } yield {
        val client: FaunaClient =
          FaunaClient.builder()
            .withSecret(secret)
            .withEndpoint("https://db.eu.fauna.com/")
            .build()

        new Fauna {
          def query[A](expr: Expr)(using d: Decoder[A]): Task[A] =
            ZIO.fromFutureJava(client.query(expr))
              .provide(Has(blocking))
              .flatMap(value => ZIO.fromEither(d.decode(value.at("data"))))
              .tapError(err => UIO(println(err.getMessage())))
        }
      }).toLayer
}
