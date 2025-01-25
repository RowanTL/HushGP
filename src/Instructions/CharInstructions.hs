module Instructions.CharInstructions where

import State
import Instructions.GenericInstructions

instructionCharConcat :: State -> State
instructionCharConcat state@(State {_char = c1 : c2 : cs, _string = ss}) = state{_char = cs, _string = [c1, c2] : ss}
instructionCharConcat state = state


