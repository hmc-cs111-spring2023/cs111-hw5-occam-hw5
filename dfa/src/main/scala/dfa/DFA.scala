package dfa // leave this line in the file

case class State(label: String)

case class Transition(from: State, to: State, symbol: Char)

class DFA(val states: Set[State], val transitions: Set[Transition],
          val start: State, val accept: Set[State]) {

    // start at the starting state
    def accepts(input: String) = acceptsHelper(input, start)

    def acceptsHelper(input: String, currentState: State): Boolean = {
        // if there is no more input, check if in an accepting state
        if input.length == 0 then accept.contains(currentState)
        else {
            // change the state based on the head character
            val nextState = transitions.filter(transition =>
                transition.from == currentState &&
                transition.symbol == input.head).head.to

            // check the rest of the input recursively
            acceptsHelper(input.tail, nextState)
        }
    }
}