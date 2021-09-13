package sol

import tester.Tester;

/**
 * Tests for graph and node classes.
 */
class GraphTestSuite {

  def testAdjacencyList(t: Tester) = {
    val drivers = new AdjacencyList[String]()
    drivers.addNode("Lewis Hamilton")
    drivers.addNode("Max Verstappen")
    drivers.addNode("Lando Norris")
    val lewis = new drivers.ALNode("Lewis Hamilton", List())
    val max = new drivers.ALNode("Max Verstappen", List())
    val lando = new drivers.ALNode("Lando Norris", List())
    drivers.addEdge(lewis, lando) // add edge from adjacency list
    lewis.addEdge(max) // add edge from node
    lando.addEdge(max) // add edge rom node
    t.checkExpect(drivers.addNode("Sebastian Vettel").getContents(), "Sebastian Vettel".hashCode)
    t.checkExpect(drivers.addNode("Pierre Gasly").getContents(), "Pierre Gasly".hashCode)
    t.checkExpect(drivers.aList.contains(lando.key)) // check that edge exists where it should
    t.checkExpect(drivers.aList(lewis.key).contains(max.key)) // check that edge exists where it should
    t.checkExpect(drivers.aList(max.key).isEmpty) // check that no edge exists where they should not
    t.checkExpect(drivers.aList(lando.key).contains(max.key)) // check that edge exists where it should
    t.checkExpect(lewis.getContents(), "Lewis Hamilton")
    t.checkExpect(lewis.getNexts().toSet.equals(Set(max, lando))) // check that the nexts are also accessible from node

  }

  def testAdjacencyMatrix(t: Tester) = {
    val drivers = new AdjacencyMatrix[String](5)
    drivers.addNode("Lewis Hamilton") // add nodes
    drivers.addNode("Max Verstappen")
    t.checkExpect(drivers.matrix(0)(1), Some(false)) // check that edge is not present where it should not be
    t.checkExpect(drivers.matrix(0)(3), None) // check that there is no possible edge where one node does not exist
    t.checkExpect(drivers.matrix(2)(3), None) // check that there is no possible edge where both nodes do not exist
    t.checkExpect(drivers.nodeCount, 2) // check that nodes were added
    drivers.addNode("Pierre Gasly")
    drivers.addNode("Sebastian Vettel")
    drivers.addNode("Lando Norris") // add more nodes
    t.checkExpect(drivers.nodeCount, 5) // check that nodes were added
    val lewis = new drivers.MNode("Lewis Hamilton", 0)
    val lando = new drivers.MNode("Lando Norris", 4)
    drivers.addEdge(lewis, lando) // add edge from matrix
    lando.addEdge(lewis) // add edge from node
    t.checkExpect(drivers.matrix(0)(4), Some(true)) // check that edge exists where it should
    t.checkExpect(drivers.matrix(4)(0), Some(true)) // check that edge exists where it should
    t.checkException(new IllegalStateException("Current node count is at maximum"), drivers, "addNode", "Yuki Tsunoda")

  }

}

object GraphTestSuite extends App {
  Tester.run(new GraphTestSuite())
}