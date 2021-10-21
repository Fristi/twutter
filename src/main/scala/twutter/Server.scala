package twutter

import cats.*
import cats.derived.*
import cats.implicits.*
import caliban.GraphQL.graphQL
import caliban.Value.StringValue
import caliban.schema.*
import caliban.{RootResolver, ZHttpAdapter}
import com.faunadb.client.query.*
import com.faunadb.client.query.Language.*
import com.faunadb.client.types.{Codec, Value}
import zhttp.http.*
import zhttp.service.Server
import zio.*
import zio.query.ZQuery
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.stream.ZStream

object TwutterApi extends GenericSchema[Has[Fauna]] {
  
  case class Tweet(message: String, by: FaunaQuery[User])
  case class User(name: String, age: Int)

  case class Queries(
    users: RIO[Has[Fauna], List[User]],
    tweets: String => RIO[Has[Fauna], List[String]],
    feed: String => RIO[Has[Fauna], List[String]]
  )

  case class CreateFollower[A](followee: A, follower: A) derives FaunaEncoder, Functor
  case class CreateTweet[A](by: A, message: String) derives FaunaEncoder, Functor

  case class Mutations(
    follow: CreateFollower[String] => RIO[Has[Fauna], Unit],
    tweet: CreateTweet[String] => RIO[Has[Fauna], Unit]
  )

  private val idxUserByName: Expr = Index("user_by_name")
  private val idxFolloweesByFollower: Expr = Index("followers_by_followee")
  private val idxTweetsByUser: Expr = Index("tweets_by_user")
  private val idxAllUsers: Expr = Index("all_users")

  private def byName(name: String): Expr =
    Select(com.faunadb.client.query.Language.Path("ref"), Get(Match(idxUserByName, new Value.StringV(name))))

  private def matchFolloweesByFollower(user: String) = Match(idxFolloweesByFollower, byName(user))
  private def matchTweetsByUser(user: String) = Match(idxTweetsByUser, byName(user))

  private def create(coll: String, expr: Expr) =
    Fauna(_.query[Value](Create(Collection(coll), Obj("data", expr))).unit)

  private val queries = Queries(
    Fauna(_.query[List[User]](Map(Paginate(Match(idxAllUsers)), Lambda("X", Get(Var("X")))))),
    user => Fauna(_.query[List[String]](Paginate(matchTweetsByUser(user)))),
    user => Fauna(_.query[List[String]](Paginate(Join(matchFolloweesByFollower(user), idxTweetsByUser))))
  )

  private val mutations = Mutations(
    req => create("relationships", req.map(byName).encode),
    req => create("tweets", req.map(byName).encode)
  )

  val api = graphQL(RootResolver(queries, mutations))
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
