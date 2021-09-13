package sol

import src.{IGraph, State}
import tester.Tester

import scala.collection.mutable.HashMap

/**
 * Tests for methods on a StateMachine.
 */
class StateMachineTestSuite {

  def testSRCData(t: Tester) = {
    val test = new StateMachine(new AdjacencyList[State])
    test.initFromCSV("data/traffic-states.csv", "data/traffic-transitions.csv")

    def bothGreen(state: State): Boolean = {
      state.data("hope") == "green" && state.data("waterman") == "green"
    }

    def hopeRedWatermanGreen(state: State): Boolean = {
      state.data("hope") == "red" && state.data("waterman") == "green"
    }

    def notPurple(state: State): Boolean = {
      state.data("hope") != "purple" && state.data("waterman") != "purple"
    }

    def purple(state: State): Boolean = {
      state.data("hope") == "purple" && state.data("waterman") == "purple"
    }

    def hopeYellowWatermanRed(state: State): Boolean = {
      state.data("hope") == "yellow" && state.data("waterman") == "red"
    }

    def bothRed(state: State): Boolean = {
      state.data("hope") == "red" && state.data("waterman") == "red"
    }

    val state6: State = test.ids("6")
    val state1: State = test.ids("1")
    val state4: State = test.ids("4")
    t.checkExpect(test.ids.knownSize, 9) // check that all 9 states were added
    t.checkExpect(test.checkAlways(state6, bothGreen).isDefined)
    t.checkExpect(test.checkAlways(state1, bothGreen).isDefined)
    t.checkExpect(test.checkAlways(state6, hopeRedWatermanGreen).isDefined)
    t.checkExpect(test.checkAlways(state1, hopeRedWatermanGreen).isDefined)
    t.checkExpect(test.checkAlways(state6, notPurple), None)
    t.checkExpect(test.checkAlways(state1, notPurple), None)
    t.checkExpect(test.checkAlways(state6, purple).isDefined)
    t.checkExpect(test.checkAlways(state1, purple).isDefined)
    t.checkExpect(test.checkEventually(state6, bothGreen), true)
    t.checkExpect(test.checkEventually(state1, bothGreen), false)
    t.checkExpect(test.checkEventually(state6, hopeRedWatermanGreen), false)
    t.checkExpect(test.checkEventually(state1, hopeRedWatermanGreen), true)
    t.checkExpect(test.checkEventually(state1, hopeYellowWatermanRed), true)
    t.checkExpect(test.checkEventually(state6, hopeYellowWatermanRed), false)
    t.checkExpect(test.checkEventually(state1, bothRed), false)
    t.checkExpect(test.checkEventually(state6, bothRed), true)
    t.checkExpect(test.checkEventually(state6, bothGreen), true)
    t.checkExpect(test.checkNever(state1, bothRed), None)
    t.checkExpect(test.checkNever(state6, bothRed).isDefined)
    t.checkExpect(test.checkNever(state6, bothGreen).isDefined)
    t.checkExpect(test.checkNever(state1, bothGreen), None)
    t.checkExpect(test.checkNever(state4, bothGreen), None)
    t.checkExpect(test.checkAlways(state4, notPurple), None)
    t.checkExpect(test.checkNever(state4, purple), None)
    t.checkExpect(test.checkEventually(state4, purple), false)
    t.checkExpect(test.checkEventually(state4, hopeYellowWatermanRed), true)
  }
  def testRelationshipMachine(t: Tester): Unit = {
    val test = new StateMachine(new AdjacencyList[State]())
    test.initFromCSV("data/drivers.csv", "data/driver-matchups.csv")
    val hamilton: State = test.ids("hamilton")
    val bottas: State = test.ids("bottas")
    val verstappen: State = test.ids("verstappen")
    val norris: State = test.ids("norris")
    val leclerc: State = test.ids("leclerc")
    val sainz: State = test.ids("sainz")
    val gasly: State = test.ids("gasly")
    val wolff: State = test.ids("wolff")
    val vettel: State = test.ids("vettel")
    val obama: State = test.ids("obama")
    val trump: State = test.ids("trump")
    val biden: State = test.ids("biden")
    t.checkExpect(test.checkAlways(hamilton, x => x.data("job") == "driver"), None)
    t.checkExpect(test.checkAlways(bottas, x => x.data("job") == "driver"), None)
    t.checkExpect(test.checkAlways(verstappen, x => x.data("job") == "driver"), None)
    t.checkExpect(test.checkAlways(norris, x => x.data("job") == "president").isDefined)
    t.checkExpect(test.checkAlways(leclerc, x => x.data("job") == "president").isDefined)
    t.checkExpect(test.checkAlways(sainz, x => x.data("job") == "president").isDefined)
    t.checkExpect(test.checkAlways(gasly, x => x.data("job") == "president").isDefined)
    t.checkExpect(test.checkNever(hamilton, x => x.data("team") == "force india"), None)
    t.checkExpect(test.checkNever(vettel, x => x.data("job") == "alpha romeo"), None)
    t.checkExpect(test.checkNever(trump, x => x.data("job") == "driver"), None)
    t.checkExpect(test.checkNever(vettel, x => x.data("job") == "alpha romeo"), None)
    t.checkExpect(test.checkEventually(wolff, x=> x.data("team") == "alphatauri"), true)
    t.checkExpect(test.checkEventually(biden, x => x.data("currently") == "no"), true)
    t.checkExpect(test.checkEventually(obama, x => x.data("currently") == "yes"), true)
  }

  def testManualAdd(t: Tester): Unit = {
    // state machine represents my grades
    val grades: IGraph[State] = new AdjacencyMatrix[State](5)
   var myGrades = new StateMachine(grades)
    val doingGreat: State =
      new State("gettingAs", Map("CS18" -> "A", "EAST520" -> "A", "IAPA500" -> "A", "MATH520" -> "A"))
    val doingOkay: State =
      new State("gettingBs", Map("CS18" -> "B", "EAST520" -> "B", "IAPA500" -> "B", "MATH520" -> "B"))
    val almostFailing: State =
      new State("gettingCs", Map("CS18" -> "C", "EAST520" -> "C", "IAPA500" -> "C", "MATH520" -> "C"))
    val failing: State =
      new State("failingAll", Map("CS18" -> "NC", "EAST520" -> "NC", "IAPA500" -> "NC", "MATH520" -> "NC"))
    val halfAsHalfBs: State =
      new State("borderline", Map("CS18" -> "A", "EAST520" -> "B", "IAPA500" -> "A", "MATH520" -> "B"))
    val notInGraph: State =
      new State("not in graph", Map("hi" -> "bye"))
    val copyofFailing: State =
      new State("failingAll", Map("hi" -> "bye"))

    myGrades.addState(halfAsHalfBs)
    myGrades.addState(doingGreat)
    myGrades.addState(doingOkay)
    myGrades.addState(almostFailing)
    myGrades.addState(failing)
    myGrades.addTransition(doingGreat, doingOkay)
    myGrades.addTransition(doingOkay, doingGreat)
    myGrades.addTransition(almostFailing, failing)
    myGrades.addTransition(failing, almostFailing)

    def predicatePassing(state: State): Boolean = {
      var passing: Boolean = true
      for (i <- state.data.keys) {
        if (state.data(i) == "NC") {passing = false}
      }
      passing
    }
    def pNotPassing(state: State): Boolean = {
      !predicatePassing(state)
    }
    def predicateF(state: State): Boolean = {
      var f: Boolean = false
      for (i <- state.data.keys) {
        if (state.data(i) == "F") {f = true}
      }
      f
    }
    t.checkExpect(myGrades.checkAlways(doingGreat, predicatePassing), None)
    t.checkExpect(myGrades.checkAlways(doingOkay, predicatePassing), None)
    t.checkExpect(myGrades.checkAlways(halfAsHalfBs, predicatePassing), None)
    t.checkExpect(myGrades.checkAlways(almostFailing, predicatePassing).isDefined)
    t.checkExpect(myGrades.checkAlways(failing, predicatePassing).isDefined)
    t.checkExpect(myGrades.checkNever(doingGreat, predicateF), None)
    t.checkExpect(myGrades.checkNever(doingOkay, predicateF), None)
    t.checkExpect(myGrades.checkNever(halfAsHalfBs, predicateF), None)
    t.checkExpect(myGrades.checkNever(almostFailing, predicateF), None)
    t.checkExpect(myGrades.checkNever(failing, predicateF), None)
    t.checkExpect(myGrades.checkEventually(doingGreat, predicatePassing), true)
    t.checkExpect(myGrades.checkEventually(doingOkay, predicatePassing), true)
    t.checkExpect(myGrades.checkEventually(halfAsHalfBs, predicatePassing), true)
    t.checkExpect(myGrades.checkEventually(almostFailing, predicatePassing), true)
    t.checkExpect(myGrades.checkEventually(doingGreat, pNotPassing), false)
    t.checkExpect(myGrades.checkEventually(doingOkay, pNotPassing), false)
    t.checkExpect(myGrades.checkEventually(halfAsHalfBs, pNotPassing), false)
    t.checkExpect(myGrades.checkEventually(almostFailing, pNotPassing), true)
    t.checkException(new IllegalStateException("Cannot add a state when state with same ID exists"),
      myGrades, "addState", copyofFailing)
  }
}

object StateMachineTestSuite extends App {
  Tester.run(new StateMachineTestSuite())
}
