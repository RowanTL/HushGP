module Instructions.StringInstructions where

import State
import Instructions.GenericInstructions

combineString :: String -> (String, String) -> String
combineString toInsert (front, back) = front <> toInsert <> back

instructionStringConcat :: State -> State
instructionStringConcat state = instructionConcat state string

instructionStringSwap :: State -> State
instructionStringSwap state = instructionSwap state string

instructionStringInsertString :: State -> State
instructionStringInsertString state@(State{_string = s1 : s2 : ss, _int = i1 : is}) = state {_string = combineString s2 (splitAt i1 s1) : ss, _int = is}
instructionStringInsertString state = state
