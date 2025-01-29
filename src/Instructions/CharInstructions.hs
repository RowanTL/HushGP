module Instructions.CharInstructions where

import Data.Char
import State
import Instructions.StringInstructions (wschars)
import Instructions.GenericInstructions

intToAscii :: Integral a => a -> Char
intToAscii val = chr (abs (fromIntegral val) `mod` 128)

instructionCharConcat :: State -> State
instructionCharConcat state@(State {_char = c1 : c2 : cs, _string = ss}) = state{_char = cs, _string = [c1, c2] : ss}
instructionCharConcat state = state

instructionCharFromFirstChar :: State -> State
instructionCharFromFirstChar state = instructionVectorFirst state char string

instructionCharFromLastChar :: State -> State
instructionCharFromLastChar state = instructionVectorLast state char string

instructionCharFromNthChar :: State -> State
instructionCharFromNthChar state = instructionVectorNth state char string

instructionCharIsWhitespace :: State -> State
instructionCharIsWhitespace state@(State {_char = c1 : cs, _bool = bs}) = state{_char = cs, _bool = (c1 `elem` wschars) : bs}
instructionCharIsWhitespace state = state

instructionCharIsLetter :: State -> State
instructionCharIsLetter state@(State {_char = c1 : cs, _bool = bs}) = state{_char = cs, _bool = isAlpha c1 : bs}
instructionCharIsLetter state = state

instructionCharIsDigit :: State -> State
instructionCharIsDigit state@(State {_char = c1 : cs, _bool = bs}) = state{_char = cs, _bool = isDigit c1 : bs}
instructionCharIsDigit state = state

instructionCharFromBool :: State -> State
instructionCharFromBool state@(State {_char = cs, _bool = b1 : bs}) = state{_char = (if b1 then 'T' else 'F') : cs, _bool = bs}
instructionCharFromBool state = state

instructionCharFromAsciiInt :: State -> State
instructionCharFromAsciiInt state@(State {_char = cs, _int = i1 : is}) = state{_char = intToAscii i1 : cs, _int = is}
instructionCharFromAsciiInt state = state

instructionCharFromAsciiFloat :: State -> State
instructionCharFromAsciiFloat state@(State {_char = cs, _float = f1 : fs}) = state{_char = intToAscii @Integer (floor f1) : cs, _float = fs}
instructionCharFromAsciiFloat state = state

instructionCharsFromString :: State -> State
instructionCharsFromString state@(State {_char = cs, _string = s1 : ss}) = state{_char = s1 <> cs, _string = ss}
instructionCharsFromString state = state
