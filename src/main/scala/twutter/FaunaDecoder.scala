package twutter

import cats.implicits.*
import com.faunadb.client.types.{Codec, Value}

import java.time.{Instant, LocalDate}
import scala.compiletime.*
import scala.deriving.*
import scala.jdk.CollectionConverters.*

enum FaunaDecodeError(error: String) extends Throwable(error):
  case FieldDoesNotExist(expr: Value, label: String) extends FaunaDecodeError(s"Field does not exists '$label' on $expr")
  case CannotDecode(expr: Value) extends FaunaDecodeError(s"Cannot decode $expr")

trait FaunaDecoder[A]:
  def decode(expr: Value): Either[FaunaDecodeError, A]


object FaunaDecoder:
  private inline def decoders[T <: Tuple]: List[FaunaDecoder[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[FaunaDecoder[t]] :: decoders[ts]

  private inline def labels[L <: Tuple]: List[String] =
    inline erasedValue[L] match
      case _ : EmptyTuple => Nil
      case _ : (name *: names) => constValue[name].toString :: labels[names]

  inline given derived[A <: Product](using m: Mirror.ProductOf[A]): FaunaDecoder[A] =
    new FaunaDecoder[A]:
      def decode(expr: Value): Either[FaunaDecodeError, A] = {
        val elems = decoders[m.MirroredElemTypes]
        val names = labels[m.MirroredElemLabels]
        val data = expr.at("data")

        elems.zip(names).traverse { case (decoder, name) =>
          val field: Value = data.at(name)

          field.orNull() match {
            case null => Left(FaunaDecodeError.FieldDoesNotExist(expr, name))
            case rest => decoder.decode(rest)
          }
        } map { res =>
          m.fromProduct(Tuple.fromArray(res.toArray)).asInstanceOf[A]
        }
      }

  private def decodeWith[A](expr: Value, codec: Codec[A]): Either[FaunaDecodeError, A] =
    Option(expr.to(codec).orNull()).fold(Left(FaunaDecodeError.CannotDecode(expr)))(Right.apply)

  given FaunaDecoder[String] with
    def decode(expr: Value): Either[FaunaDecodeError, String] = decodeWith(expr, Codec.STRING)

  given FaunaDecoder[Instant] with
    def decode(expr: Value): Either[FaunaDecodeError, Instant] = decodeWith(expr, Codec.TIME)

  given FaunaDecoder[LocalDate] with
    def decode(expr: Value): Either[FaunaDecodeError, LocalDate] = decodeWith(expr, Codec.DATE)

  given FaunaDecoder[Boolean] with
    def decode(expr: Value): Either[FaunaDecodeError, Boolean] = decodeWith(expr, Codec.BOOLEAN).map(_.booleanValue())

  given FaunaDecoder[Double] with
    def decode(expr: Value): Either[FaunaDecodeError, Double] = decodeWith(expr, Codec.DOUBLE).map(_.toDouble)

  given FaunaDecoder[Long] with
    def decode(expr: Value): Either[FaunaDecodeError, Long] = decodeWith(expr, Codec.LONG).map(_.toLong)

  given FaunaDecoder[Float] with
    def decode(expr: Value): Either[FaunaDecodeError, Float] = decodeWith(expr, Codec.FLOAT).map(_.toFloat)

  given FaunaDecoder[Int] with
    def decode(expr: Value): Either[FaunaDecodeError, Int] = decodeWith(expr, Codec.INTEGER).map(_.toInt)

  given FaunaDecoder[Value] with
    def decode(expr: Value): Either[FaunaDecodeError, Value] = Right(expr)

  given [A](using d: FaunaDecoder[A]): FaunaDecoder[List[A]] with
    def decode(expr: Value): Either[FaunaDecodeError, List[A]] = decodeWith(expr, Codec.ARRAY).flatMap(_.asScala.toList.traverse(d.decode))


