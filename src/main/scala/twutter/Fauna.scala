package twutter


import cats.implicits.*
import com.faunadb.client.FaunaClient
import com.faunadb.client.query.Expr
import com.faunadb.client.types.{Codec, Value}
import zio.*
import zio.blocking.Blocking
import zio.stream.ZStream

import java.time.{Instant, LocalDate}

trait Fauna {
  def query[A](expr: Expr)(using d: FaunaDecoder[A]): Task[A]
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
          def query[A](expr: Expr)(using d: FaunaDecoder[A]): Task[A] =
            ZIO.fromFutureJava(client.query(expr))
              .provide(Has(blocking))
              .flatMap(value => ZIO.fromEither(d.decode(value.at("data"))))
              .tapError(err => UIO(println(err.getMessage())))
        }
      }).toLayer
}
