
package cs.purdue.edu.quadtree

import scala.collection.mutable.{ArrayBuffer, PriorityQueue}

/**
 * Some useful constants that we don't want to hardcode.
 */
object Constants {
  // $COVERAGE-OFF$
  @inline final val MaxEntries = 50
  // $COVERAGE-ON$
}

import Constants._

/**
 * Abstract data type that has a geom element.
 *
 * This generalizes Node[A] (the nodes of the tree) and Entry[A] (the
 * values being put in the tree). It functions like a structural type
 * (but isn't one, because structural types are slow).
 */
sealed abstract class HasGeom {
  def geom: Geom
}

/**
 * this is the node for the general node
 * @tparam A
 */
sealed abstract class Node[A] extends HasGeom {self =>


  def box: Box
  def geom: Geom = box

  /**
   * we assume the children store the nodes from
   * 0,          1,          2,          3
   * north-east, south-east, south-west, north-west
   */
  def children: Vector[HasGeom]


  /**
   * Put all the entries this node contains (directly or indirectly)
   * into a vector. Obviously this could be quite large in the case of
   * a root node, so it should not be used for traversals.
   */
  def entries: Vector[Entry[A]] = {
    val buf = ArrayBuffer.empty[Entry[A]]
    def recur(node: Node[A]): Unit = node match {
      case Leaf(children, _) =>
        buf ++= children
      case Branch(children, _) =>
        children.foreach(recur)
    }
    recur(this)
    buf.toVector
  }

  /**
   * Returns an iterator over all the entires this node contains
   * (directly or indirectly). Since nodes are immutable there is no
   * concern over concurrent updates while using the iterator.
   */
  def iterator: Iterator[Entry[A]] = this match {
    case Leaf(children, _) =>
      children.iterator
    case Branch(children, _) =>
      children.iterator.flatMap(_.iterator)
  }

  /**
   * Method to pretty-print an r-tree.
   *
   * This method should only be called on small-ish trees! It will
   * print one line for every branch, leaf, and entry, so for a tree
   * with thousands of entries this will result in a very large
   * string!
   */
  def pretty: String = {
    def prettyRecur(node: Node[A], i: Int, sb: StringBuilder): Unit = {
      val pad = " " * i
      val a = node.box.area
      node match {
        case lf @ Leaf(children, box) =>
          val pad2 = " " * (i + 1)
          sb.append(s"$pad leaf $a $box:\n")
          children.foreach { case Entry(pt, value) =>
            sb.append(s"$pad2 entry $pt: $value\n")
          }
        case Branch(children, box) =>
          sb.append(s"$pad branch $a $box:\n")
          children.foreach(c => prettyRecur(c, i + 1, sb))
      }
    }
    val sb = new StringBuilder
    prettyRecur(this, 0, sb)
    sb.toString
  }


  /**
   * Insert a new Entry into the tree.
   *
   * Since this node is immutable, the method will return a
   * replacement. There are two possible situations:
   *
   * 1. We can replace this node with a new node. This is the common
   *    case.
   *
   * 2. This node was already "full", so we can't just replace it with
   *    a single node. Instead, we will split this node into
   *    (presumably) two new nodes, and return a vector of them.
   *
   * The reason we are using vector here is that it simplifies the
   * implementation, and also because eventually we may support bulk
   * insertion, where more than two nodes might be returned.
   */
  def insert(entry: Entry[A]): Either[Vector[Node[A]], Node[A]] = {

    //1. for the leaf node
    //if the capacity is smaller than the max
    //insert this node into this leaf node
    //else
    //split this leaf node and insert into one of the leaf node

    //2. for the branch node
    //find the best branch child node to insert
    //insert the new node into the best node

    this match {
      case Leaf(children, box) =>
        val cs = children :+ entry
        if (cs.length <= MaxEntries) {
          Right(Leaf(cs, box.expand(entry.geom)))
        } else {
          Left(Node.splitLeaf(cs))
        }

      case Branch(children, box) =>

        //find the best child to insert
        val pt = entry.geom
        val halfx=(box.x2-box.x)/2
        val halfy=(box.y2-box.y)/2
        var node=children(0)
        var n=0

        //little stupid to work as this way
        //we only assume there are four children for each branch node
        if(pt.x<box.x+halfx)
        {
          if(pt.y<box.y+halfy) {
            node = children(2)
            n = 2
          }
          else {
            node = children(3)
            n = 3
          }
        }
        else
        {
          if(pt.y<box.y+halfy) {
            node = children(1)
            n = 1
            //Pnode.se
          }else {
            //Pnode.ne
            node = children(0)
            n = 0
          }
        }

        //do the actual insertion for the branch node
        node.insert(entry) match
        {
            //return a vector
          case Left(rs) =>
            val cs = children.take(n) ++ children.drop(n + 1) ++ rs
            if (cs.length <= MaxEntries) {
              val b = rs.foldLeft(box)(_ expand _.box)
              Right(Branch(cs, b))
            } else {
              Left(Node.splitBranch(cs))
            }
            //return a node
          case Right(r) =>
            val cs = children.updated(n, r)
            if (cs.length <= MaxEntries) {
              Right(Branch(children.updated(n, r), box.expand(r.box)))
            } else {
              Left(Node.splitBranch(cs))
            }

        }

        //insert



    }


  }



  /**
   * Search for all entries given a search space, spatial checking
   * function, and criteria function.
   *
   * This method abstracts search and searchIntersection, where the
   * `check` function is either space.contains or space.intersects,
   * respectively.
   */
  def genericSearch(space: Box, check: Geom => Boolean, f: Entry[A] => Boolean): Seq[Entry[A]] = {
    if (!space.isFinite) Nil
    else {
      val buf = ArrayBuffer.empty[Entry[A]]
      def recur(node: Node[A]): Unit = node match {
        case Leaf(children, box) =>
          children.foreach { c =>
            if (check(c.geom) && f(c)) buf.append(c)
          }
        case Branch(children, box) =>
          children.foreach { c =>
            if (space.intersects(box)) recur(c)
          }
      }
      if (space.intersects(box)) recur(this)
      buf
    }
  }


  /**
   * Find the closest entry to `pt` that is within `d0`.
   *
   * This method will either return Some((distance, entry)) or None.
   */
  def nearest(pt: Point, d0: Double): Option[(Double, Entry[A])] = {


  }

  /**
   * Find the closest `k` entries to `pt` that are within `d0`, and
   * add them to the given priority queue `pq`.
   *
   * This method returns the distance of the farthest entry that is
   * still included.
   */
  def nearestK(pt: Point, k: Int, d0: Double, pq: PriorityQueue[(Double, Entry[A])]): Double =
  {

  }


  /**
   * Count the number of entries contained within `space`.
   */
  def count(space: Box): Int =
    if (!space.isFinite) 0 else {
      def recur(node: Node[A]): Int = node match {
        case Leaf(children, box) =>
          var n = 0
          var i = 0
          while (i < children.length) {
            if (space.contains(children(i).geom)) n += 1
            i += 1
          }
          n
        case Branch(children, box) =>
          var n = 0
          var i = 0
          while (i < children.length) {
            val c = children(i)
            if (space.intersects(c.box)) n += recur(c)
            i += 1
          }
          n
      }
      if (space.intersects(box)) recur(this) else 0
    }




}


/**
 * this the leaf node of quadtree
 * @param children
 * @param box
 * @tparam A
 */
case class Leaf[A](children: Vector[Entry[A]], box: Box) extends Node[A] {


}

/**
 * this is the internal node of quadtree
 * @param children
 * @param box
 * @tparam A
 */
case class Branch[A](children: Vector[Node[A]], box: Box) extends Node[A] {


}

/**
 * Represents a point with a value.
 *
 * We frequently use value.== so it's important that A have a
 * reasonable equality definition. Otherwise things like remove and
 * contains may not work very well.
 */
case class Entry[A](geom: Geom, value: A) extends HasGeom

object Node {

  /**
   * Splits the children of a leaf node.
   *
   * See splitter for more information.
   */
  def splitLeaf[A](children: Vector[Entry[A]]): Vector[Leaf[A]] = {
    //val ((es1, box1), (es2, box2)) = splitter(children)
    //Vector(Leaf(es1, box1), Leaf(es2, box2))

    //splitter chilern

    //split the children
    //default number of leaf childern is four
    //vector(Leaf(es1, box1), Leaf(es2, box2),Leaf(es3, box3), Leaf(es4, box4))

  }


}