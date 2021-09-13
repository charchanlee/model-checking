package sol

import src.{IGraph, INode}

import scala.collection.mutable.HashMap

/**
 * Adjacency matrix implementation of an IGraph.
 * @param maxSize the maximum number of nodes in the graph
 * @tparam T the type of the nodes contents
 */
class AdjacencyMatrix[T](val maxSize: Integer) extends IGraph[T] {

  if (maxSize <= 0) {throw new IllegalArgumentException}
  // this keeps a record of key value pairs, keys = index, value = associated node
  var nodeRecord: HashMap[Int, INode[T]]= new HashMap()
  // how many nodes exist in the system
  var nodeCount: Int = 0
  // adjacency matrix
  var matrix: Array[Array[Option[Boolean]]]= Array.ofDim(maxSize, maxSize)

  // fill array with Nones (no edges exist yet)
  for (i <- matrix.indices) {
    for (j <- matrix(i).indices) {
      matrix(i)(j) = None
    }
  }

  override def addNode(contents: T): INode[T] = {
    // check to see if there are too many nodes already
    if (nodeCount == maxSize) {throw new IllegalStateException("Current node count is at maximum")}
    else {
      // update the node count
      nodeCount = nodeCount + 1
      // create a new node with index (zero indexed)
      val newNode = new MNode(contents, nodeCount - 1)
      // add the node to the node record
      nodeRecord.addOne(newNode.i, newNode)
      // change all links from "None"s to "Some(false)" for that node
      // this means that an edge could exist, but it does not
      for (i <- matrix.indices) {
        for (j <- matrix.indices)
          if (nodeRecord.contains(i) && nodeRecord.contains(j)) {matrix(i)(j) = Some(false)}
      }
      newNode
    }
  }

  override def addEdge(fromNode: INode[T], toNode: INode[T]): Unit = {
    // type casting is necessary here because we must use the i field (index)
    // this does not exist in INode
    matrix(fromNode.asInstanceOf[MNode].i)(toNode.asInstanceOf[MNode].i) = Some(true)
  }


  override def show(): Unit = {
    for (i <- matrix.indices) {
      var l: List[Int] = List()
      for (j <- matrix(i).indices) {
        if (matrix(i)(j).contains(true)) {l = j :: l}
      }
      println(i.toString ++ "points to: " ++ l.toString)
    }
  }

  /**
   * Class for a node in an AdjacencyMatrix graph.
   * @param contents - the node's contents
   * @param index - the index in the matrix that corresponds to this node
   */
  class MNode(val contents: T, val index: Int) extends INode[T] {

    val i = index

    override def getContents(): T = this.contents

    override def getNexts(): List[INode[T]] = {
      var nodeList = List()
      for (i <- matrix.indices) {
        for (j <- matrix(i).indices) {
          if (matrix(i)(j).contains(true)) {nodeRecord(j) :: nodeList}
        }
      }
      nodeList
    }

    override def addEdge(toNode: INode[T]): Unit = {
      matrix(i)(toNode.asInstanceOf[MNode].i) = Some(true)
    }

    override def toString: String = {
      i.toString
    }

  }
}
