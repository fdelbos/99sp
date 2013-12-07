// 
// lists.scala
// 
// Created by Frederic DELBOS - fred.delbos@gmail.com on Dec  6 2013.
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// 

package com.fdelbos.NinetyNineSP
import scala.util.Random

object Lists {

  def last(lst: List[Any]): Option[Any] = lst match {
    case Nil => None
    case h :: Nil => Some(h)
    case h :: t => last(t)
  }

  def penultimate(lst: List[Any]): Option[Any] =  lst match {
    case h :: t :: Nil => Some(h)
    case h :: t => penultimate(t)
    case _ => None
  }

  def nth(n: Int, lst: List[Any]): Option[Any] = lst match {
    case h :: t if n == 0 => Some(h)
    case h :: t if n >= 0 => nth(n - 1, t)
    case _ => None
  }

  def length(lst: List[Any]): Int = lst match {
    case h :: t => 1 + length(t)
    case _ => 0
  }

  def reverse(lst: List[Any]): List[Any] = {
    def comp(ori: List[Any], rev: List[Any]): List[Any] = ori match {
      case h :: t => comp(t, h :: rev)
      case _ => rev
    }
    comp(lst, Nil)
  }

  def isPalindrome(lst: List[Any]): Boolean = {
    def comp(l1: List[Any], l2: List[Any]): Boolean = (l1, l2) match {
      case (Nil, Nil) => true
      case (h1 :: t1, h2 :: t2) if h1 == h2  => comp(t1, t2)
      case _ => false
    }
    comp(lst, reverse(lst))
  }

  def flatten(lst: List[Any]): List[Any] = lst match {
    case (l: List[Any]) :: t => flatten(l) ++ flatten(t)
    case h :: t => h :: flatten(t)
    case _ => Nil
  }

  def compress(lst: List[Any]): List[Any] = lst match {
    case a :: b :: t if a == b => compress(b :: t)
    case h :: t => h :: compress(t)
    case _ => Nil
  }

  def pack(lst: List[Any]): List[Any] = {
    def together(l1: List[Any], l2: List[Any]): List[Any] = l1 match {
      case h1 :: t1 => l2 match {
        case h2 :: t2 if h1 == h2 => together(h1 :: h2 :: t1, t2)
        case h2 :: t2 if h1 != h2 => l1 :: together(List(h2), t2)
        case _ => List(l1)
      }
      case _ => Nil
    }
    lst match {
      case h :: t => together(List(h), t)
      case _ => Nil
    }
  }

  def encode(lst: List[Any]): List[Any] = {
    def build(lst: List[Any]): List[Any] = lst match {
      case l1 :: lt => l1 match {
        case h :: t => (length(h :: t), h) :: build(lt)
        case _ => Nil
      }
      case _ => Nil
    }
    build(pack(lst))
  }

  def encodeModified(lst: List[Any]): List[Any] = {
    def build(lst: List[Any]): List[Any] = lst match {
      case l1 :: lt => l1 match {
        case h :: t => length(h :: t) match {
          case 1 => h :: build(lt)
          case l => (l, h) :: build(lt)
        }
        case _ => Nil
      }
      case _ => Nil
    }
    build(pack(lst))
  }

  def decode(lst: List[(Int, Any)]): List[Any] = {
    def gen(size: Int, data: Any): List[Any] = size match {
      case s if s > 0 => data :: gen(s - 1, data)
      case _ => Nil
    }
    lst match {
      case (size, data) :: t => gen(size, data) ++ decode(t)
      case _ => Nil
    }
  }

  def encodeDirect(lst: List[Any]) = {
    def build(a: Any, count: Int, lst: List[Any]): List[(Int, Any)] = lst match {
      case h :: t if a == h => build(a, count + 1, t)
      case h :: t if a != h => (count, a) :: build(h, 1, t)
      case _ => (count, a) :: Nil
    }
    lst match {
      case h :: t => build(h, 1, t)
      case _ => Nil
    }
  }

  def duplicate(lst: List[Any]): List[Any] = lst match {
    case h :: t => h :: h :: duplicate(t)
    case _ => Nil
  }

  def duplicateN(n: Int, lst: List[Any]): List[Any] = {
    def clone(n: Int, e: Any): List[Any] = n match {
      case 0 => Nil
      case _ => e :: clone(n - 1, e)
    }
    lst match {
      case h :: t => clone(n, h) ++ duplicateN(n, t)
      case _ => Nil
    }
  }

  def drop(n: Int, lst: List[Any]): List[Any] = {
    def remove(pos: Int, lst: List[Any]): List[Any] = lst match {
      case h :: t if pos % n == 0 => remove(pos + 1, t)
      case h :: t => h :: remove(pos + 1, t)
      case _ => Nil
    }
    remove(1, lst)
  }

  def split(n: Int, lst: List[Any]): (List[Any], List[Any]) = (n, lst) match {
    case (0, l) => (Nil, l)
    case (n, h :: t) => 
      val (pre, post) = split(n - 1, t)
      (h :: pre, post)
    case _ => (Nil, Nil)
  }

  def slice(bgn: Int, end: Int, lst: List[Any]): List[Any] = (bgn, end, lst) match {
    case (_, 0, _) => Nil
    case (0, _, h :: t) => h :: slice(0, end - 1, t)
    case (_, _, h :: t) if bgn > 0 => slice(bgn - 1, end - 1, t)
  }

  def rotate(r: Int, lst: List[Any]): List[Any] = (r, lst) match {
    case (0, _) => lst
    case (_, h :: t) if r > 0 => rotate(r - 1, t ++ List(h))
    case (_, _) if r < 0 => rotate(length(lst) + r, lst)
  }

  def removeAt(at: Int, lst: List[Any]): (List[Any], Any) = (at, lst) match {
    case (0, h :: t) => (t, h)
    case (_, h :: t) => 
      val (l, e) = removeAt(at - 1, t)
      (h :: l, e)
    case _ => throw new NoSuchElementException
  }

  def insertAt(e: Any, at: Int, lst: List[Any]): List[Any] = (at, lst) match {
    case (0, _) => e :: lst
    case (_, h :: t) => h :: insertAt(e, at - 1, t)
    case (_, Nil) => throw new NoSuchElementException
  }

  def range(bgn: Int, end: Int): List[Int] =  {
    if (bgn < end) bgn :: range(bgn + 1, end)
    else if (bgn > end) bgn :: range(bgn - 1, end)
    else bgn :: Nil
  }

  def randomSelect(n: Int, lst: List[Any]): List[Any] = (n, lst) match {
    case (_, _) if n < 0 => throw new NoSuchElementException
    case (0, _) => Nil
    case (_, _) => 
      removeAt(Random.nextInt(length(lst)), lst) match {
        case (l, e) => e :: randomSelect(n - 1, l)
      }
  }

  def lotto(n: Int, m: Int) = randomSelect(n, range(1, m))

  def randomPermute(lst: List[Any]) = randomSelect(length(lst), lst)

  def combinations(n: Int, lst: List[Any]): List[Any] = {
    def build(bgn: List[Any], n: Int, lst: List[Any]): List[Any] = (n, lst) match {
      case (0, h :: t) => List(reverse(h :: bgn)) ++ build(bgn, 0, t)
      case (_, h :: t) => build(h :: bgn, n - 1, t) ++ build(bgn, n, t)
      case (_, Nil) => Nil
    }
    build(Nil, n - 1, lst)
  }

  // def group(sizes: List[Int], lst: List[Any]): List[Any] = {
  //   def gen(sizes: List[Int], lst: List[Any]) = sizes match {
  //     case h :: t => List(combinations(h, lst) ++ group(t, lst))
  //     case _ => Nil
  //   }
  //   gen(sizes, lst) match {
  //     case h :: t => h
  //     case _ => Nil
  //   }
  // }

  // def group(sizes: List[Int], lst: List[Any]): List[Any] = {
    


  //   def loopFor(sizes: List[Int], current: List[List[Any]], lst: List[Any]) = (size, current) match {
  //     case (sh :: st, ch :: ct) => 
  //       available = flatten(ch) diff lst
  //       gen(, available)
  //   }

  //   def gen(sizes: List[Int], curr: List[List[Any]], lst: List[Any]): List[Any] = (size) match {
  //     case (Nil) => Nil
  //     case (sh :: st) => 


  //   }
  // }

}
