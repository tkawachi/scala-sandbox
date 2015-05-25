package net.pikot

import scalaz.Free.FreeC
import scalaz.std.function._
import scalaz.{Free, ~>}

sealed trait Console[A]

case object ReadLine extends Console[Option[String]]

case class PrintLn(s: String) extends Console[Unit]

case object NOP extends Console[Unit]

object Console {
  type ConsoleIO[A] = FreeC[Console, A]

  def readLine: ConsoleIO[Option[String]] = Free.liftFC(ReadLine)

  def printLn(s: String): ConsoleIO[Unit] = Free.liftFC(PrintLn(s))

  def nop: ConsoleIO[Unit] = Free.liftFC(NOP)

  val interpreterF0 = new (Console ~> Function0) {
    override def apply[A](fa: Console[A]): () => A = fa match {
      case ReadLine => () => Option(scala.io.StdIn.readLine())
      case PrintLn(s) => () => println(s)
      case NOP => () => ()
    }
  }
}

object ConsoleMain {

  def main(args: Array[String]): Unit = {
    val program = for {
      x <- Console.readLine
      _ <- x.map(Console.printLn).getOrElse(Console.nop)
    } yield x

    val fun = Free.runFC(program)(Console.interpreterF0)
    fun()
  }
}
