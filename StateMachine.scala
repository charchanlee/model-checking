package sol

import org.apache.commons.csv.CSVRecord
import src.{IGraph, INode, IStateMachine, Parser, State}

import scala.collection.mutable
import scala.collection.mutable.HashMap

/**
 * A class for a state machine.
 * @param stateGraph - a specific graph implementation of type State
 */
class StateMachine(stateGraph: IGraph[State]) extends IStateMachine {

  var states: HashMap[State, Set[State]] = new HashMap()
  var ids: HashMap[String, State] = new HashMap()

    override def addState(state: State): Unit = {
      if (ids.contains(state.id)) {
        throw new IllegalStateException("Cannot add a state when state with same ID exists")
      } else {
        states.put(state, Set())
        ids.put(state.id, state)
      }
    }

  override def addTransition(fromState: State, toState: State): Unit = {
    if (ids.contains(fromState.id) && ids.contains(toState.id)) {
      states.update(fromState, states(fromState) + toState)
    } else {
      throw new IllegalStateException("Cannot add transition when one or more of states do not yet exist")
    }
  }
  def checkHelper(startState: State, checkState: State => Boolean, visited: Set[State]): Option[State] = {
    var visitedStates: Set[State] = visited
    if (checkState(startState)) {
      var answer: Option[State] = None
      val nextStates: Set[State] = states(startState)
      for (next <- nextStates) {
        if (!visitedStates.contains(next)) {
          visitedStates = visited + next
          if (checkHelper(next, checkState, visitedStates).isDefined) {answer = checkAlways(next, checkState)}
        } else {answer = None}
      }
      answer
    } else {Some(startState)}
  }
  def checkEventuallyHelper(startState: State, checkState: State => Boolean, visited: Set[State]): Boolean = {
    var visitedStates: Set[State] = visited
    checkState(startState)  || {
      val nextStates: Set[State] = states(startState)
      var answer: Boolean = false
      for (next <- nextStates) {
        if (!visitedStates.contains(next)) {
          visitedStates = visited + next
          if (checkEventuallyHelper(next, checkState, visitedStates)) {answer = true}
        } else false
      }
      answer
    }
  }

  override def checkAlways(startState: State, checkState: State => Boolean): Option[State] = {
    checkHelper(startState, checkState, Set())
  }
  override def checkNever(startState: State, checkState: State => Boolean): Option[State] = {
    checkHelper(startState, x => !checkState(x), Set())
  }

  override def checkEventually(startState: State, checkState: State => Boolean): Boolean = {
    checkEventuallyHelper(startState, checkState, Set())
  }

  override def show(): Unit = {
    println("there are " ++ states.knownSize.toString ++ "states/n")
    for (i <- states.keys) {
      println("state: " ++ i.toString ++ "points to: " ++ states(i).toString ++ "/n")
    }
  }

  override def initFromCSV(statesCSV: String, transitionsCSV: String): Unit = {
    Parser.parseCSV(statesCSV, statesHelper)
    Parser.parseCSV(transitionsCSV, transitionsHelper)

  }
  def statesHelper(input: CSVRecord): Unit = {
    // create an empty map which will go into the data field of the state
    var newData: Map[String, String] = Map()
    //check that the the id does not yet exist in the state machine
    if (!ids.contains(input.get(0))) {
      //this will create a new array in which each element is in the format key=value
      val data: Array[String] = input.get(1).split(";")
      // loop through the array
      for (i <- data.indices){
        // extract the key and value by removing the equals sign
        // add this as a key value pair into the newData map
        newData = newData + (data(i).split("=")(0) -> data(i).split("=")(1))
      }
      // create a new state using the map just constructed
      val newState: State = new State(input.get(0), newData)
      // record this state to have no edges (initially)
      // record this (id, state) as a key value pair in the ids hashmap
      states.put(newState, Set())
      ids.put(newState.id, newState)
    } else {throw new IllegalStateException("Cannot add a state when state with same ID exists")}

  }

  def transitionsHelper(input: CSVRecord): Unit = {
    if (ids.contains(input.get(0)) && ids.contains(input.get(1))) {
      val fromState: State = ids(input.get(0))
      val toState: State = ids(input.get(1))
      states.update(fromState, states(fromState) + toState)
    } else {throw new IllegalStateException("Cannot add transition when one or more of states do not yet exist")}
  }


}