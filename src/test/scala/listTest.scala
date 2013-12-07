// 
// TestLists.scala
// 
// Created by Frederic DELBOS - fred.delbos@gmail.com on Dec  6 2013.
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// 

import org.scalatest._
import org.scalatest.matchers._

import com.fdelbos.NinetyNineSP._

class ListsTest extends FlatSpec with ShouldMatchers {

  "P01" should "Find the last element of a list." in {
    assert(Lists.last(List(1, 2 ,3 ,4 ,5 ,6)).get == 6)
    assert(Lists.last(List()) == None)
  }

  "P02" should "Find the last but one element of a list." in {
    assert(Lists.penultimate(List(1, 2 ,3 ,4 ,5 ,6)).get == 5)
    assert(Lists.penultimate(List(1)) == None)
    assert(Lists.penultimate(List()) == None)
  }

  "P03" should "Find the Kth element of a list." in {
    assert(Lists.nth(3, List(1, 2 ,3 ,4 ,5 ,6)).get == 4)
    assert(Lists.nth(0, List(1, 2 ,3 ,4 ,5 ,6)).get == 1)
    assert(Lists.nth(5, List(1, 2 ,3 ,4 ,5 ,6)).get == 6)
    assert(Lists.nth(-4, List(1, 2 ,3 ,4 ,5 ,6)) == None)
    assert(Lists.nth(8, List(1, 2 ,3 ,4 ,5 ,6)) == None)
    assert(Lists.nth(0, List()) == None)
  }

  "P04" should "Find the number of elements of a list." in {
    assert(Lists.length(List(1, 2 ,3 ,4 ,5 ,6)) == 6)
    assert(Lists.length(List()) == 0)
  }

  "P05" should "Reverse a list." in {
    assert(Lists.reverse(List(1, 2 ,3 ,4 ,5 ,6)) == List(6, 5, 4, 3, 2, 1))
    assert(Lists.reverse(List()) == Nil)
  }

  "P06" should "Find out whether a list is a palindrome." in {
    assert(Lists.isPalindrome(List(1, 2 ,3 ,2 ,1)) == true)
    assert(Lists.isPalindrome(List(1)) == true)
    assert(Lists.isPalindrome(List(1, 2)) == false)
  }

  "P07" should "Flatten a nested list structure." in {
    assert(Lists.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
  }

  "P08" should "Eliminate consecutive duplicates of list elements." in {
    assert(Lists.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
  }

  "P09" should "Pack consecutive duplicates of list elements into sublists." in {
    assert(Lists.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ==
      List(
        List('a, 'a, 'a, 'a),
        List('b),
        List('c, 'c),
        List('a, 'a),
        List('d),
        List('e, 'e, 'e, 'e)))
    assert(Lists.pack(List()) == Nil)
    assert(Lists.pack(List('a')) == List(List('a')))
  }

  "p10" should "Run-length encoding of a list." in {
    assert(Lists.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  "P11" should "Modified run-length encoding." in {
    assert(Lists.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }

  "P12" should "Decode a run-length encoded list." in {
     assert(Lists.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  "P13" should "Run-length encoding of a list (direct solution)." in {
    assert(Lists.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ==
    List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  "P14" should "Duplicate the elements of a list." in {
    assert(Lists.duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  "P15" should "Duplicate the elements of a list a given number of times." in {
    assert(Lists.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  "P16" should "Drop every Nth element from a list." in {
    assert(Lists.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  "P17" should "Split a list into two parts." in {
    assert(Lists.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  "P18" should "Extract a slice from a list." in {
    assert(Lists.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g))
  }

  "P19" should "Rotate a list N places to the left." in {
    assert(Lists.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    assert(Lists.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  "P20" should "Remove the Kth element from a list." in {
    assert(Lists.removeAt(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd), 'b))
  }

  "P21" should "Insert an element at a given position into a list." in {
    assert(Lists.insertAt('new, 1, List('a, 'b, 'c, 'd)) == List('a, 'new, 'b, 'c, 'd))
  }

  "P22" should "Create a list containing all integers within a given range." in {
    assert(Lists.range(4, 9) == List(4, 5, 6, 7, 8, 9))
    assert(Lists.range(9, 4) == List(9, 8, 7, 6, 5, 4))
  }

  scala.util.Random.setSeed(42) // for consistent calls to random

  "P23" should "Extract a given number of randomly selected elements from a list." in {
    assert(Lists.randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)) == List('b, 'f, 'g))
  }

  "P24" should "Lotto: Draw N different random numbers from the set 1..M." in {
    assert(Lists.lotto(6, 49) == List(32, 19, 22, 6, 43, 49))
  }

  "P25" should "Generate a random permutation of the elements of a list." in {
    assert(Lists.randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)) == List('f, 'c, 'b, 'a, 'd, 'e))
  }

  "P26" should "Generate the combinations of K distinct objects chosen from the N elements of a list." in {
    assert(Lists.combinations(3, List('a, 'b, 'c, 'd, 'e)) == 
      List(
        List('a, 'b, 'c), 
        List('a, 'b, 'd), 
        List('a, 'b, 'e),
        List('a, 'c, 'd),
        List('a, 'c, 'e),
        List('a, 'd, 'e),
        List('b, 'c, 'd),
        List('b, 'c, 'e),
        List('b, 'd, 'e),
        List('c, 'd, 'e)))
  }

  
  "P27" should "Group the elements of a set into disjoint subsets." in {
    //info(Lists.group(List(2, 2), List(1, 2, 3, 4)).toString)
  }
}
