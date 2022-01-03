package com.example

import zio.{ ZIO, Exit, UIO }
import zio.stm.{ STM, ZSTM, TRef }

import munit.FunSuite

final class ZSTMTest extends FunSuite {

  final object example {

    //     4                       6
    //      \                     / \
    //       6                  4     8
    //      / \        ~>        \   / \
    //     5   8                  5 7   9
    //        / \
    //       7   9

    def makeExampleTree: STM[Nothing, BinTree[String]] = {
      for {
        n7 <- BinTree.makeBranch(7, "seven")
        n9 <- BinTree.makeBranch(9, "nine")
        n5 <- BinTree.makeBranch(5, "five")
        n8 <- BinTree.makeBranch(8, "eight")
        _ <- n8.left.set(n7) *> n8.right.set(n9)
        n6 <- BinTree.makeBranch(6, "six")
        _ <- n6.left.set(n5) *> n6.right.set(n8)
        n4 <- BinTree.makeBranch(4, "four")
        _ <- n4.right.set(n6)
        t <- BinTree.newFromBranch(n4)
      } yield t
    }

    def rotateExampleTree(t: BinTree[String]): STM[Exception, Unit] = {
      t.rotateLeftAtRoot
    }
  }

  final val N = 1024

  // This test deadlocks:
  test("BinTree concurrent rotate and lookups".only) {
    concurrentRotateAndLookups()
  }

  private[this] def logExitTask[A](okMsg: Option[String])(exit: Exit[Throwable, A]): UIO[Unit] = {
    exit.fold(
      failed = { c =>
        ZIO.attempt(println(c.toString())).orDie
      },
      completed = { _ =>
        okMsg.fold(ifEmpty = UIO.unit)(msg => ZIO.attempt(println(msg)).orDie)
      },
    )
  }

  def concurrentRotateAndLookups(): Unit = {
    import BinTree._
    val tsk = for {
      t <- example.makeExampleTree.commit
      rotateTask = t.rotateLeftAtRoot.commit.resurrect.onExit(logExitTask(Some("rotateLeftAtRoot OK")))
      lookupTasks = ZIO.foreachPar((1 to 10).flatMap(_ => List(5, 8, 7, 9))) { n =>
        t.lookup(n).map(n -> _).commit.resurrect.onExit(logExitTask(None))
      }.replicateZIO(N).map(_.flatten)
      f1 <- (ZIO.yieldNow *> rotateTask).forkDaemon
      f2 <- lookupTasks.forkDaemon
      _ <- f1.join
      r2 <- f2.join
      _ <- ZIO.attempt {
        r2.foreach { kr =>
          assert(clue(kr)._2.isDefined)
        }
      }
    } yield ()
    zio.Runtime.default.unsafeRunTask(tsk.replicateZIODiscard(N))
  }

  test("BinTree concurrent lookups") {
    concurrentLookups()
  }

  def concurrentLookups(): Unit = {
    import BinTree._
    val tsk = for {
      t <- example.makeExampleTree.commit
      lookupTasks = ZIO.foreachPar((1 to 10).flatMap(_ => List(5, 8, 7, 9))) { n =>
        t.lookup(n).map(n -> _).commit
      }.replicateZIO(N).map(_.flatten)
      f <- lookupTasks.forkDaemon
      r <- f.join
      _ <- ZIO.attempt {
        r.foreach { kr =>
          assert(clue(kr)._2.isDefined)
        }
      }
    } yield ()
    zio.Runtime.default.unsafeRunTask(tsk.replicateZIODiscard(N))
  }

  test("BinTree lookup") {
    import BinTree._
    val tsk = for {
      t <- example.makeExampleTree.commit
      _ <- t.lookup(4).commit.flatMap(r => ZIO.attempt(assertEquals(r, Some("four"))))
      _ <- t.lookup(6).commit.flatMap(r => ZIO.attempt(assertEquals(r, Some("six"))))
      _ <- t.lookup(5).commit.flatMap(r => ZIO.attempt(assertEquals(r, Some("five"))))
      _ <- t.lookup(8).commit.flatMap(r => ZIO.attempt(assertEquals(r, Some("eight"))))
      _ <- t.lookup(7).commit.flatMap(r => ZIO.attempt(assertEquals(r, Some("seven"))))
      _ <- t.lookup(9).commit.flatMap(r => ZIO.attempt(assertEquals(r, Some("nine"))))
      _ <- t.height.commit.flatMap(r => ZIO.attempt(assertEquals(r, 4)))
    } yield ()
    zio.Runtime.default.unsafeRunTask(tsk)
  }

  test("BinTree rotate") {
    import BinTree._
    val tsk = for {
      t <- example.makeExampleTree.commit
      _ <- t.debugString.commit.flatMap { s => ZIO.attempt(println(s)) }
      _ <- t.height.commit.flatMap(r => ZIO.attempt(assertEquals(r, 4)))
      _ <- t.rotateLeftAtRoot.commit
      _ <- t.debugString.commit.flatMap { s => ZIO.attempt(println(s)) }
      _ <- t.lookup(4).commit.flatMap(r => ZIO.attempt(assertEquals(r, Some("four"))))
      _ <- t.lookup(6).commit.flatMap(r => ZIO.attempt(assertEquals(r, Some("six"))))
      _ <- t.lookup(5).commit.flatMap(r => ZIO.attempt(assertEquals(r, Some("five"))))
      _ <- t.lookup(8).commit.flatMap(r => ZIO.attempt(assertEquals(r, Some("eight"))))
      _ <- t.lookup(7).commit.flatMap(r => ZIO.attempt(assertEquals(r, Some("seven"))))
      _ <- t.lookup(9).commit.flatMap(r => ZIO.attempt(assertEquals(r, Some("nine"))))
      _ <- t.height.commit.flatMap(r => ZIO.attempt(assertEquals(r, 3)))
    } yield ()
    zio.Runtime.default.unsafeRunTask(tsk)
  }
}
