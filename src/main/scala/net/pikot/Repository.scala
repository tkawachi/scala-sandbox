package net.pikot


import scala.collection.mutable
import scala.language.higherKinds
import scalaz.Free.FreeC
import scalaz.Id.Id
import scalaz._

trait Repository[ID, Entity] {

  def lift[F[_],G[_],A](f: F[A])(implicit I: Inject[F,G]): FreeC[G,A] =
    Free.liftFC(I.inj(f))

  def or[F[_], H[_], G[_]](
                            fg: F ~> G, hg: H ~> G
                            ): ({ type f[x] = Coproduct[F, H, x]})#f ~> G =
    new (({type f[x] = Coproduct[F,H,x]})#f ~> G) {
      def apply[A](c: Coproduct[F,H,A]): G[A] = c.run match {
        case -\/(fa) => fg(fa)
        case \/-(ha) => hg(ha)
      }
    }
  sealed abstract class RepositoryDSL[A]

  case class ResolveBy(id: ID) extends RepositoryDSL[Option[Entity]]

  case class Store(id: ID, entity: Entity) extends RepositoryDSL[Entity]

  class RepositoryMethods[F[_]](implicit I: Inject[RepositoryDSL, F]) {
    def resolveBy(id: ID): FreeC[F, Option[Entity]] = lift(ResolveBy(id))
    def store(id: ID, entity: Entity): FreeC[F, Entity] = lift(Store(id, entity))
  }

  object RepositoryMethods {
    implicit def instance[F[_]](implicit I: Inject[RepositoryDSL, F]): RepositoryMethods[F] =
      new RepositoryMethods[F]
  }
}

case class FooEntity(id: Long, name: String)

trait FooRepository extends Repository[Long, FooEntity] {

  sealed abstract class FooRepositoryDSL[A]

  case class ResolveByName(name: String) extends FooRepositoryDSL[Option[FooEntity]]

  class FooRepositoryMethods[F[_]](implicit I: Inject[FooRepositoryDSL, F]) {
    def resolveByName(name: String): FreeC[F, Option[FooEntity]] = lift(ResolveByName(name))
  }

  object FooRepositoryMethods {
    implicit def instance[F[_]](implicit I: Inject[FooRepositoryDSL, F]): FooRepositoryMethods[F] =
      new FooRepositoryMethods[F]
  }
}

class FooRepositoryOnMemory(map: mutable.Map[Long, FooEntity]) extends FooRepository {
  def dslInterpreter = new (RepositoryDSL ~> Id) {
    override def apply[A](fa: RepositoryDSL[A]): Id[A] = fa match {
      case ResolveBy(id) =>
        map.get(id)
      case Store(id, entity) =>
        map.put(id, entity)
        entity
    }
  }

  def fooDslInterpreter = new (FooRepositoryDSL ~> Id) {
    override def apply[A](fa: FooRepositoryDSL[A]): Id[A] = fa match {
      case ResolveByName(name) => map.values.find(_.name == name)
    }
  }

  def fooInterpreter = or(dslInterpreter, fooDslInterpreter)
}

object RepositoryMain {
  def main(args: Array[String]): Unit = {
    val repo = new FooRepositoryOnMemory(mutable.Map.empty)

    def prg[F[_]](implicit R: repo.RepositoryMethods[F], FO: repo.FooRepositoryMethods[F]) = {
      import R._
      import FO._
      for {
        _ <- store(1, FooEntity(1, "abc"))
        e <- resolveByName("abc")
      } yield e
    }

    type App[A] = Coproduct[repo.RepositoryDSL, repo.FooRepositoryDSL, A]
    val app = prg[App]

    val result = Free.runFC(app)(repo.fooInterpreter)
    println(result)
  }
}
