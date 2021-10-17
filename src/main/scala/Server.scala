import zhttp.http.*
import zio.{ExitCode, Has, RIO, ZEnv, ZIO, App}
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.stream.ZStream
import caliban.GraphQL.graphQL
import caliban.schema.GenericSchema
import caliban.{RootResolver, ZHttpAdapter}
import zhttp.service.Server
import com.faunadb.client.query.Language.*
import com.faunadb.client.types.Codec

object TwutterApi extends GenericSchema[Has[Fauna]] {
  case class User(name: String, age: Int)
  case class Queries(users: RIO[Has[Fauna], List[User]])

  val queries = Queries(Fauna(_.query[List[User]](Map(Paginate(Match(Index("all_users"))), Lambda("X", Get(Var("X")))))))

  val api = graphQL(RootResolver(queries))
}

object TwutterServer extends App {
  private val graphiql = Http.succeed(Response.http(content = HttpData.fromStream(ZStream.fromResource("graphiql.html"))))

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      interpreter <- TwutterApi.api.interpreter
      _           <- Server
        .start(
          8088,
          Http.route {
            case _ -> Root / "api" / "graphql" => ZHttpAdapter.makeHttpService(interpreter)
            case _ -> Root / "ws" / "graphql"  => ZHttpAdapter.makeWebSocketService(interpreter)
            case _ -> Root / "graphiql"        => graphiql
          }
        )
        .forever
    } yield ())
      .provideSomeLayer(ZEnv.live >+> Fauna.layer(System.getenv("FAUNA_SECRET")))
      .exitCode
}
