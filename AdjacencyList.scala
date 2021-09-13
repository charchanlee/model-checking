package sol

import src.{IGraph, INode}

import scala.collection.mutable.HashMap

/**
 * Class for the adjacency list representation of IGraph.
 *
 * @tparam T the type of the nodes' contents
 */
class AdjacencyList[T] extends IGraph[T] {

  // adjacency list is a table of hashcodes to set of hashcodes
  // key -> value : hashcode of node -> set of node hashcodes that it points to
  // each node has a hashcode identifier
  var aList = new HashMap[Int, Set[Int]]

  override def addNode(contents: T): INode[T] = {
    val newNode = new ALNode(contents, List()) // the new node has no edges
    aList.addOne(newNode.key, Set())
    newNode
  }

  override def addEdge(fromNode: INode[T], toNode: INode[T]): Unit = {
    val fNode = fromNode.asInstanceOf[ALNode]
    val tNode = toNode.asInstanceOf[ALNode]
    aList.update(fNode.key, aList(fNode.key) + tNode.key)
    fNode.nodeList = toNode :: fNode.nodeList

  }

  override def show(): Unit = {
    for (i <- aList.keys) {
      println("key:" ++ i.toString ++ "points to: " ++ aList(i).toString() ++ "/n")
    }
  }

  /**
   * Inner class for a node in an AdjacencyList graph.
   * @param contents the contents of the node
   * @param getsTo the list of nodes which the current node outbounds to
   */
  class ALNode(val contents: T, var getsTo: List[INode[T]]) extends INode[T] {

    var c: T = contents
    var nodeList: List[INode[T]] = getsTo
    val key = contents.hashCode()

    override def getContents(): T = {
      c
    }

    override def getNexts(): List[INode[T]] = {
      nodeList
    }

    override def addEdge(toNode: INode[T]): Unit = {
      nodeList = toNode :: nodeList
      aList.update(this.key, aList(this.key) + toNode.asInstanceOf[ALNode].key)


    }

    override def toString = {
      this.key.toString
    }
  }
}
