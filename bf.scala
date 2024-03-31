 //> using scala "3.3.3"
package mybf

import scala.annotation.tailrec

enum BFCommand:
	case Increment
	case Decrement
	case MoveRight
	case MoveLeft
	case Input
	case Output
	case Loop(content: (List[BFCommand]))
end BFCommand

def parse(s: String) = 
	def loop(stack: List[BFCommand], s: List[Char]): List[BFCommand] = s match
		case '+'::rest => loop(BFCommand.Increment::stack, rest)
		case '-'::rest => loop(BFCommand.Decrement::stack, rest)
		case '>'::rest => loop(BFCommand.MoveRight::stack, rest)
		case '<'::rest => loop(BFCommand.MoveLeft::stack, rest)
		case ','::rest => loop(BFCommand.Input::stack, rest)
		case '.'::rest => loop(BFCommand.Output::stack, rest)
		case '['::rest =>
			val (loopContent, remaining) = rest.span(_ != ']')
			loop(BFCommand.Loop(loop(Nil, loopContent))::stack, remaining.tail)
		case Nil => stack.reverse
		case _ => throw new IllegalArgumentException(s"Unexpected char ${s}")
	end loop
	loop(Nil, s.toList.filterNot(c => c.isWhitespace))
end parse

case class BFState(
	memory: Array[Int],
	pointer: Int,
	input: List[Char],
	output: List[Char],
	program: List[BFCommand]
)

@tailrec
def eval(state: BFState): BFState = state.program match
	case Nil => state
	case BFCommand.Increment::rest =>
		state.memory(state.pointer) += 1
		eval(state.copy(program = rest))
	case BFCommand.Decrement::rest =>
		state.memory(state.pointer) -= 1
		eval(state.copy(program = rest))
	case BFCommand.MoveRight::rest =>
		eval(state.copy(pointer = state.pointer + 1, program = rest))
	case BFCommand.MoveLeft::rest =>
		eval(state.copy(pointer = state.pointer - 1, program = rest))
	case BFCommand.Output::rest =>
		eval(state.copy(output = state.memory(state.pointer).toChar::state.output, program = rest))
	case BFCommand.Input::rest =>
		eval(state.copy(memory = state.memory.updated(state.pointer, state.input.head.toInt), input = state.input.tail, program = rest))
	case BFCommand.Loop(loopContent)::rest =>
		if (state.memory(state.pointer) == 0)
		then eval(state.copy(program = rest))
		else eval(state.copy(program = loopContent ++ (BFCommand.Loop(loopContent)::rest)))
	case _ => throw new IllegalArgumentException(s"unexpected command ${state.program}")
end eval

def run(program: String, input: String): String =
	val commands = parse(program)
	val initialState = BFState(
		Array.fill(30000)(0),
		0,
		input.toList,
		Nil,
		commands
	)
	val finalState = eval(initialState)
	finalState.output.reverse.mkString
end run

@main
def main(program: String, input: String = ""): Unit =
	println(run(program, input))
	

	
