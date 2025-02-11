module HushGP.Instructions.CharInstructions where

import Data.Char
import HushGP.State
import HushGP.Instructions.GenericInstructions
import HushGP.Instructions.Utility

-- |Combines the top two chars into a string and pushes the result to the string stack.
instructionCharConcat :: State -> State
instructionCharConcat state@(State {_char = c1 : c2 : cs, _string = ss}) = state{_char = cs, _string = [c1, c2] : ss}
instructionCharConcat state = state

-- |Takes the first char from the top string and pushes it to the char stack.
-- If the string is empty, acts as a no-op.
instructionCharFromFirstChar :: State -> State
instructionCharFromFirstChar = instructionVectorFirst char string

-- |Takes the last char from the top string and pushes it to the char stack.
-- If the string is empty, acts as a no-op.
instructionCharFromLastChar :: State -> State
instructionCharFromLastChar = instructionVectorLast char string

-- |Takes the Nth char from the top string and pushes it to the char stack
-- based on the top int from the int stack. If the string is empty, acts as a no-op.
instructionCharFromNthChar :: State -> State
instructionCharFromNthChar = instructionVectorNth char string

-- |Takes the top of the char stack, checks to see if it is whitespace, and then
-- pushes True to the bool stack if so, else false.
instructionCharIsWhitespace :: State -> State
instructionCharIsWhitespace state@(State {_char = c1 : cs, _bool = bs}) = state{_char = cs, _bool = (c1 `elem` wschars) : bs}
instructionCharIsWhitespace state = state

-- |Takes the top of the char stack, checks to see if it is an alphabetic character, and
-- then pushes True to the bool stack if alphabetic, false if not.
instructionCharIsLetter :: State -> State
instructionCharIsLetter state@(State {_char = c1 : cs, _bool = bs}) = state{_char = cs, _bool = isAlpha c1 : bs}
instructionCharIsLetter state = state

-- |Takes the top of the char stack, checks to see if it is a digit, and then pushes True if it is
-- a digit, False if not.
instructionCharIsDigit :: State -> State
instructionCharIsDigit state@(State {_char = c1 : cs, _bool = bs}) = state{_char = cs, _bool = isDigit c1 : bs}
instructionCharIsDigit state = state

-- |Takes the top of the bool stack, pushes 'T' to the char stack if True, 'F' to the char stack if False.
instructionCharFromBool :: State -> State
instructionCharFromBool state@(State {_char = cs, _bool = b1 : bs}) = state{_char = (if b1 then 'T' else 'F') : cs, _bool = bs}
instructionCharFromBool state = state

-- |Takes the top of the int stack, pushes the ascii representation of the int to the char stack.
instructionCharFromAsciiInt :: State -> State
instructionCharFromAsciiInt state@(State {_char = cs, _int = i1 : is}) = state{_char = intToAscii i1 : cs, _int = is}
instructionCharFromAsciiInt state = state

-- |Takes the top of the float stack, pushes the ascii representation of the floored float to the char stack.
instructionCharFromAsciiFloat :: State -> State
instructionCharFromAsciiFloat state@(State {_char = cs, _float = f1 : fs}) = state{_char = intToAscii @Integer (floor f1) : cs, _float = fs}
instructionCharFromAsciiFloat state = state

-- |Pushes the top string to the char stack split up into individual chars.
-- For example: have the string "hello" and the char stack ['a', 'b', 'c'], the char stack
-- looks like ['h', 'e', 'l', 'l', 'o', 'a', 'b', 'c'] after this instruction executes.
instructionCharsFromString :: State -> State
instructionCharsFromString state@(State {_char = cs, _string = s1 : ss}) = state{_char = s1 <> cs, _string = ss}
instructionCharsFromString state = state

-- |Pops the top of the char stack.
instructionCharPop :: State -> State
instructionCharPop = instructionPop char

-- |Duplicates the top of the char stack.
instructionCharDup :: State -> State
instructionCharDup = instructionDup char

-- |Duplicates the top of the char stack N times based on the top of
-- int stack.
instructionCharDupN :: State -> State
instructionCharDupN = instructionDupN char

-- |Swaps the top two chars of the char stack.
instructionCharSwap :: State -> State
instructionCharSwap = instructionSwap char

-- |Rotates the top three chars of the char stack.
instructionCharRot :: State -> State
instructionCharRot = instructionRot char

-- |Sets the char stack to [].
instructionCharFlush :: State -> State
instructionCharFlush = instructionFlush char

-- |Checks to see if the top two chars to equal and pushes the result
-- to the bool stack.
instructionCharEq :: State -> State
instructionCharEq = instructionEq char

-- |Calculates the stack depth of the char stack. Pushes the result
-- to the int stack.
instructionCharStackDepth :: State -> State
instructionCharStackDepth = instructionStackDepth char

-- |Moves an item from deep within the char stack to the top of the char stack based on
-- the top int from the int stack.
instructionCharYank :: State -> State
instructionCharYank = instructionYank char

-- |Copies an item from deep within the char stack to the top of the char stack based on
-- the top int from the int stack.
instructionCharYankDup :: State -> State
instructionCharYankDup = instructionYankDup char

-- |Pushes True to the bool stack if the char stack is empty. False if not.
instructionCharIsStackEmpty :: State -> State
instructionCharIsStackEmpty = instructionIsStackEmpty char

-- |Moves an item from the top of the char stack to deep within the char stack based on
-- the top int from the int stack.
instructionCharShove :: State -> State
instructionCharShove = instructionShove char

-- |Copies an item from the top of the char stack to deep within the char stack based on
-- the top int from the int stack.
instructionCharShoveDup :: State -> State
instructionCharShoveDup = instructionShoveDup char

-- |Duplicate the top N items from the char stack based on the top int from the int stack.
instructionCharDupItems :: State -> State
instructionCharDupItems = instructionDupItems char

-- |Takes the top string from the string stack and invidually pushes
-- all chars in said string to the char stack.
instructionCharFromAllString :: State -> State
instructionCharFromAllString = instructionPushAll char string
