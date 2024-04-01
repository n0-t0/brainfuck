 //> using scala "3.3.3"
package mybf

import scala.annotation.tailrec
import scala.collection.mutable

enum BFCommand:
	case Increment
	case Decrement
	case MoveRight
	case MoveLeft
	case Input
	case Output
	case Loop(content: List[BFCommand])
end BFCommand

 
def extractLoop(chars: List[Char]): (List[Char], List[Char]) =
  @tailrec
  def loop(chars: List[Char], depth: Int, acc: List[Char]): (List[Char], List[Char]) = chars match {
    case ']' :: rest if depth == 1 =>
      (acc, rest)
    case ']' :: rest =>
      loop(rest, depth - 1, acc :+ ']')
    case '[' :: rest =>
      loop(rest, depth + 1, acc :+ '[')
    case c :: rest =>
      loop(rest, depth, acc :+ c)
    case Nil =>
      throw new IllegalArgumentException(s"Unmatched '[' in program")
  }
  loop(chars, 1, Nil)
end extractLoop


def parse(s: String): List[BFCommand] = {
  def loop(stack: List[BFCommand], s: List[Char]): List[BFCommand] = s match
    case '+'::rest =>
      loop(BFCommand.Increment::stack, rest)
    case '-'::rest =>
      loop(BFCommand.Decrement::stack, rest)
    case '>'::rest =>
      loop(BFCommand.MoveRight::stack, rest)
    case '<'::rest =>
      loop(BFCommand.MoveLeft::stack, rest)
    case ','::rest =>
      loop(BFCommand.Input::stack, rest)
    case '.'::rest =>
      loop(BFCommand.Output::stack, rest)
    case '['::rest =>
      val (loopContent, remaining) = extractLoop(rest)
      loop(BFCommand.Loop(loop(Nil, loopContent))::stack, remaining)
    case Nil => 
      stack.reverse
    case _ =>
      throw new IllegalArgumentException(s"Unexpected char ${stack.head}")
  end loop

  loop(Nil, s.toList.filterNot(_.isWhitespace))
}


class BFState(
	var memory: mutable.ArraySeq[Int],
	var pointer: Int,
	var input: mutable.Stack[Char],
	var output: mutable.Stack[Char],
	var program: mutable.Stack[BFCommand]
)

def eval(state: BFState): BFState =
  while !state.program.isEmpty do
    val top = state.program.pop() 
    top match
      case BFCommand.Increment =>
        state.memory(state.pointer) += 1
      case BFCommand.Decrement =>
        state.memory(state.pointer) -= 1
      case BFCommand.MoveRight =>
        state.pointer += 1
      case BFCommand.MoveLeft =>
        state.pointer -= 1
      case BFCommand.Output =>
        state.output.push(state.memory(state.pointer).toChar)
      case BFCommand.Input =>
        state.memory(state.pointer) = state.input.pop().toInt
      case BFCommand.Loop(loopContent) =>
        if (state.memory(state.pointer) != 0) then
          state.program.push(BFCommand.Loop(loopContent))
          state.program.pushAll(loopContent.reverse)
  return state
end eval

def run(program: String, input: String): String =
  val commands = parse(program)
  var initialState = BFState(
		mutable.ArraySeq.fill(10000000)(0),
		0,
		mutable.Stack(input: _*),
		mutable.Stack.empty,
		mutable.Stack.from(commands)
	)
  val finalState = eval(initialState)
  finalState.output.reverse.mkString
end run

@main
def main(program: String, input: String = ""): Unit =
	println(run(program, input))
	

	
