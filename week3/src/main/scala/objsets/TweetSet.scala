package objsets

import TweetReader._

class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    s"User: $user\nText: $text [$retweets]"

  def maxRetweeted(that: Tweet): Tweet =
    if (this.retweets < that.retweets)
      that
    else
      this

  def < (that: Tweet): Boolean = text < that.text
  def > (that: Tweet): Boolean = text > that.text
}

abstract class TweetSet {
  def filter(p: Tweet => Boolean): TweetSet

  def union(that: TweetSet): TweetSet
  
  def mostRetweeted: Tweet
  
  def descendingByRetweet: TweetList
  
  def incl(tweet: Tweet): TweetSet

  def remove(tweet: Tweet): TweetSet

  def contains(tweet: Tweet): Boolean

  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {
  def filter(p: Tweet => Boolean): TweetSet = this
  
  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  def union(that: TweetSet) = that

  def mostRetweeted: Tweet = throw new NoSuchElementException

  def descendingByRetweet: TweetList = Nil
}

class NonEmpty(val elem: Tweet, val left: TweetSet, val right: TweetSet) extends TweetSet {
  def filter(p: Tweet => Boolean): TweetSet =
    if (p(elem))
      new NonEmpty(elem, left.filter(p), right.filter(p))
    else
      left.filter(p).union(right.filter(p))
  
  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet =
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  def union(that: TweetSet): TweetSet = that match {
    case other: NonEmpty if other.elem > elem =>
      new NonEmpty(elem, left.union(other.left), right.incl(other.elem).union(other.right))
    case other: NonEmpty if other.elem < elem =>
      new NonEmpty(elem, left.incl(other.elem).union(other.left), right.union(other.right))
    case other: NonEmpty =>
      new NonEmpty(elem, left.union(other.left), right.union(other.right))
    case _: Empty =>
      this
  }

  def mostRetweeted: Tweet = (left, right) match {
    case (_: Empty, _: Empty) =>
      elem
    case (_: Empty, right: NonEmpty) =>
      elem.maxRetweeted(right.mostRetweeted)
    case (left: NonEmpty, _: Empty) =>
      elem.maxRetweeted(left.mostRetweeted)
    case _ =>
      elem.maxRetweeted(left.mostRetweeted.maxRetweeted(right.mostRetweeted))
  }

  def descendingByRetweet: TweetList = {
    val head = mostRetweeted
    new Cons(head, remove(head).descendingByRetweet)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(t => google.exists(t.text.contains))

  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(t => apple.exists(t.text.contains))

  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
