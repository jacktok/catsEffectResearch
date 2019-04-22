import java.io._

import cats.effect.concurrent.Semaphore
import cats.effect.{Concurrent, IO, Resource, Sync}
import cats.implicits._

object MainResearch {

  def inputStream(file: File, guard: Semaphore[IO]): Resource[IO, FileInputStream] = Resource
    .make(IO(new FileInputStream(file)))(out => guard.withPermit(IO(out.close()).handleErrorWith(_ => IO.unit)))

  def outputStream(file: File, guard: Semaphore[IO]): Resource[IO, FileOutputStream] = Resource
    .make(IO(new FileOutputStream(file)))(in => guard.withPermit(IO(in.close()).handleErrorWith(_ => IO.unit)))

  def inputOutputStream(in: File, out: File, guard: Semaphore[IO]): Resource[IO, (FileInputStream, FileOutputStream)] = {
    for {
      inStream <- inputStream(in, guard)
      outStream <- outputStream(out, guard)
    } yield (inStream, outStream)
  }

  def transmit(origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): IO[Long] = {
    for {
      amount <- Sync[IO].delay(origin.read(buffer, 0, buffer.length))
      count <- if (amount > -1) IO(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
      else Sync[IO].pure(acc)
    } yield count
  }

  def transfer(origin: InputStream, destination: OutputStream): IO[Long] = {
    for {
      buffer <- IO(new Array[Byte](1024 * 10))
      total <- transmit(origin, destination, buffer, 0L)
    } yield total
  }

  def copy(origin: File, destination: File)(implicit concurrent: Concurrent[IO]): IO[Long] = {
    for {
      guard <- Semaphore[IO](1)
      count <- inputOutputStream(origin, destination, guard).use { case (in, out) =>
        guard.withPermit(transfer(in, out)
        )
      }
    } yield count
  }

  /* def copy(origin: File, destination: File): IO[Long] = {
     val inIO = IO(new FileInputStream(origin))
     val outIO = IO(new FileOutputStream(destination))
     (inIO, outIO)
       .tupled
       .bracket {
         case (in, out) => transfer(in, out)
       } {
         case (in, out) => (IO(in.close()), IO(out.close())).tupled.handleErrorWith(_ => IO.unit).void
       }
   }*/
}
