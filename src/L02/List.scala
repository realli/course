package L02

// + Complete the 10 exercises below by filling out the function bodies.
//   Replace the function bodies (sys.error("todo")) with an appropriate solution.
// + These exercises may be done in any order, however:
//   Exercises are generally increasing in difficulty, though some people may find later exercise easier.
// + Note the existence of the library function max :: Int -> Int -> Int which will help you with Exercise 9.
// + Bonus for using the provided functions or for using one exercise solution to help solve another.
// + Approach with your best available intuition; just dive in and do what you can!

// TOTAL marks:    /66

sealed trait List[A] {
  final def foldRight[B](f: (A, B) => B, b: B): B =
    this match {
      case Nil() => b
      case h|:t => f(h, t.foldRight(f, b))
    }

  @annotation.tailrec
  final def foldLeft[B](f: (B, A) => B, b: B): B =
    this match {
      case Nil() => b
      case h|:t => t.foldLeft(f, f(b, h))
    }

  def headOr(a: => A): A =
    sys.error("todo")

  override def toString: String =
    foldRight[scala.List[A]](_ :: _, scala.Nil).toString
}
case class Nil[A]() extends List[A]
case class |:[A](h: A, t: List[A]) extends List[A]

/*
module L02.List where

import Test.QuickCheck


reduceRight :: (a -> a -> a) -> List a -> a
reduceRight _ Nil      = error "bzzt. reduceRight on empty list"
reduceRight f (h :| t) = foldRight f h t

reduceLeft :: (a -> a -> a) -> List a -> a
reduceLeft _ Nil      = error "bzzt. reduceLeft on empty list"
reduceLeft f (h :| t) = foldLeft f h t

-- END Helper functions and data types

-- BEGIN Exercises

-- Exercise 1
-- Relative Difficulty: 1
-- Correctness: 2.0 marks
-- Performance: 0.5 mark
-- Elegance: 0.5 marks
-- Total: 3
headOr :: List a -> a -> a
headOr = error "todo"

-- Exercise 2
-- Relative Difficulty: 2
-- Correctness:   2.5 marks
-- Performance: 1 mark
-- Elegance: 0.5 marks
-- Total: 4
suum :: List Int -> Int
suum = error "todo"

-- Exercise 3
-- Relative Difficulty: 2
-- Correctness: 2.5 marks
-- Performance: 1 mark
-- Elegance: 0.5 marks
-- Total: 4
len :: List a -> Int
len = error "todo"

-- Exercise 4
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.0 mark
-- Elegance: 1.5 marks
-- Total: 7
maap :: (a -> b) -> List a -> List b
maap = error "todo"

-- Exercise 5
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
fiilter :: (a -> Bool) -> List a -> List a
fiilter = error "todo"

-- Exercise 6
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
append :: List a -> List a -> List a
append = error "todo"

-- Exercise 7
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
flatten :: List (List a) -> List a
flatten = error "todo"

-- Exercise 8
-- Relative Difficulty: 7
-- Correctness: 5.0 marks
-- Performance: 1.5 marks
-- Elegance: 1.5 mark
-- Total: 8
flatMap :: (a -> List b) -> List a -> List b
flatMap = error "todo"

-- Exercise 9
-- Relative Difficulty: 8
-- Correctness: 3.5 marks
-- Performance: 2.0 marks
-- Elegance: 3.5 marks
-- Total: 9
seqf :: List (a -> b) -> a -> List b
seqf = error "todo"

-- Exercise 10
-- Relative Difficulty: 10
-- Correctness: 5.0 marks
-- Performance: 2.5 marks
-- Elegance: 2.5 marks
-- Total: 10
rev :: List a -> List a
rev = error "todo"

-- Exercise 10.1
-- How to produce arbitrary instances of List
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap (foldr (:|) Nil) arbitrary

-- END Exercises
*/