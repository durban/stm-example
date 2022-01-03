package com.example

import zio.stm._

final class BinTree[A] private (root: TRef[BinTree.Branch[A]]) {

  def lookup(key: Int): STM[Nothing, Option[A]] =
    BinTree.lookup(root, key)

  def height: STM[Nothing, Int] =
    BinTree.height(root)

  def rotateLeftAtRoot: STM[Exception, Unit] =
    BinTree.rotateLeft(root)

  def debugString: STM[Nothing, String] =
    BinTree.debugStr(root)
}

object BinTree {

  def newEmpty[A]: STM[Nothing, BinTree[A]] =
    TRef.make[BinTree.Branch[A]](null).map(tr => new BinTree(tr))

  private[example] def newFromBranch[A](b: Branch[A]): STM[Nothing, BinTree[A]] =
    TRef.make[BinTree.Branch[A]](b).map(tr => new BinTree(tr))

  private[example] final class Branch[A](
    val key: Int,
    val value: A,
    val left: TRef[Branch[A]],
    val right: TRef[Branch[A]]
  )

  private def lookup[A](ref: TRef[Branch[A]], key: Int): STM[Nothing, Option[A]] = {
    ref.get.flatMap { tree =>
      if (tree eq null) {
        STM.succeed(None)
      } else {
        if (key < tree.key) lookup(tree.left, key)
        else if (key > tree.key) lookup(tree.right, key)
        else STM.succeed(Some(tree.value))
      }
    }
  }

  private def height[A](ref: TRef[Branch[A]]): STM[Nothing, Int] = {
    ref.get.flatMap { tree =>
      if (tree eq null) {
        STM.succeed(0)
      } else {
        for {
          lh <- height(tree.left)
          rh <- height(tree.right)
        } yield (lh max rh) + 1
      }
    }
  }

  private def rotateLeft[A](rootRef: TRef[Branch[A]]): STM[Exception, Unit] = {
    for {
      root <- rootRef.get
      _ <- if (root eq null) STM.fail(new IllegalArgumentException("root is null")) else STM.unit
      pivot <- root.right.get
      _ <- if (pivot eq null) STM.fail(new IllegalArgumentException("pivot is null")) else STM.unit
      pivotLeft <- pivot.left.get
      _ <- root.right.set(pivotLeft)
      _ <- pivot.left.set(root)
      _ <- rootRef.set(pivot)
    } yield ()
  }

  private[example] def makeBranch[A](k: Int, v: A): STM[Nothing, Branch[A]] = {
    for {
      l <- TRef.make[Branch[A]](null)
      r <- TRef.make[Branch[A]](null)
    } yield new Branch(k, v, l, r)
  }

  private def debugStr[A](ref: TRef[Branch[A]]): STM[Nothing, String] = {
    // we're cheating here:
    val sb = new StringBuilder
    val spaces = "  "
    def go(ref: TRef[Branch[A]], indent: Int): STM[Nothing, Unit] = {
      ref.get.flatMap { tree =>
        if (tree eq null) {
          STM.unit.map { _ => sb.append(spaces.repeat(indent) + "⊥") }
        } else {
          STM.unit.flatMap { _ =>
            sb.append(spaces.repeat(indent) + "Branch(\n")
            sb.append(spaces.repeat(indent + 1) + s"${tree.key} -> ${tree.value},\n")
            go(tree.left, indent + 1).flatMap { _ =>
              sb.append(",\n")
              go(tree.right, indent + 1).map { _ =>
                sb.append(",\n")
                sb.append(spaces.repeat(indent) + ")")
              }
            }
          }
        }
      }
    }
    go(ref, indent = 0).as(sb.result())
  }
}
