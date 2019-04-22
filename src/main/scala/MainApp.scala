import java.io.File

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
object MainApp extends IOApp{
  override def run(args: List[String]): IO[ExitCode] = {

    for {
    _ <- if (args.length < 2) IO.raiseError(new IllegalArgumentException("Need origin and destination files")) else IO.unit
      origin = new File(args.head)
      destination = new File(args(1))
      count <- MainResearch.copy(origin, destination)
      _ <- IO(println(s"$count bytes copied from ${origin.getPath} to ${destination.getPath}"))
    } yield ExitCode.Success
  }
}
