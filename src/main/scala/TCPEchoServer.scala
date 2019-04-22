import java.io.{BufferedReader, BufferedWriter, InputStreamReader, PrintWriter}
import java.net.{ServerSocket, Socket}

import cats.effect.syntax.all._
import cats.effect.{Concurrent, ExitCase, ExitCode, IO, IOApp, Resource, Sync}
import cats.implicits._

object TCPEchoServer extends IOApp {

  def echoProtocol[F[_] : Sync](clientSocket: Socket): F[Unit] = {
    def loop(reader: BufferedReader, writer: BufferedWriter): F[Unit] = for {
      line <- Sync[F].delay(reader.readLine())
      _ <- line match {
        case "" => Sync[F].unit
        case _ => Sync[F].delay {
          writer.write(line)
          writer.newLine()
          writer.flush()
        } >> loop(reader, writer)
      }
    } yield ()

    def reader(clientSocket: Socket): Resource[F, BufferedReader] = {
      Resource.make(Sync[F].delay(new BufferedReader(new InputStreamReader(clientSocket.getInputStream)))) {
        reader =>
          Sync[F].delay(reader.close()).handleErrorWith(_ => Sync[F].unit)
      }
    }


    def writer(clientSocket: Socket): Resource[F, BufferedWriter] = {
      Resource.make(Sync[F].delay(new BufferedWriter(new PrintWriter(clientSocket.getOutputStream)))) {
        writer =>
          Sync[F].delay(writer.close()).handleErrorWith(_ => Sync[F].unit)
      }
    }

    def readerWriter(clientSocket: Socket): Resource[F, (BufferedReader, BufferedWriter)] = {
      for {
        reader <- reader(clientSocket)
        writer <- writer(clientSocket)
      } yield (reader, writer)
    }

    readerWriter(clientSocket).use { case (reader, writer) =>
      loop(reader, writer)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    def close[F[_] : Sync](serverSocket: ServerSocket): F[Unit] = Sync[F].delay(serverSocket.close()).handleErrorWith(_ => Sync[F].unit)

    IO(new ServerSocket(args.headOption.map(_.toInt).getOrElse(5555)))
      .bracket { serverSocket => serve[IO](serverSocket) >> IO.pure(ExitCode.Success)} {
        serverSocket => close[IO](serverSocket) >> IO(println("server finished"))
      }
  }

  def serve[F[_] : Concurrent](serverSocket: ServerSocket): F[Unit] = {
    def close(socket: Socket): F[Unit] = Sync[F].delay(socket.close()).handleErrorWith(_ => Sync[F].unit)

    for {
      _ <- Sync[F]
        .delay(serverSocket.accept())
        .bracketCase { socket =>
          echoProtocol(socket)
            .guarantee(close(socket))
            .start
        } { (socket, exit) =>
          exit match {
            case ExitCase.Completed => Sync[F].unit
            case ExitCase.Error(_) | ExitCase.Canceled => close(socket)
          }
        }
      _ <- serve(serverSocket)
    } yield ()

  }
}
