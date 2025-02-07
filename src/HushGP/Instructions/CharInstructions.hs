module HushGP.Instructions.CharInstructions where

import Data.Char
import HushGP.State
import HushGP.Instructions.StringInstructions (wschars)
import HushGP.Instructions.GenericInstructions

intToAscii :: Integral a => a -> Char
intToAscii val = chr (abs (fromIntegral val) `mod` 128)

instructionCharConcat :: State -> State
instructionCharConcat state@(State {_char = c1 : c2 : cs, _string = ss}) = state{_char = cs, _string = [c1, c2] : ss}
instructionCharConcat state = state

instructionCharFromFirstChar :: State -> State
instructionCharFromFirstChar = instructionVectorFirst char string

instructionCharFromLastChar :: State -> State
instructionCharFromLastChar = instructionVectorLast char string

instructionCharFromNthChar :: State -> State
instructionCharFromNthChar = instructionVectorNth char string

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

instructionCharPop :: State -> State
instructionCharPop = instructionPop char

instructionCharDup :: State -> State
instructionCharDup = instructionDup char

instructionCharDupN :: State -> State
instructionCharDupN = instructionDupN char

instructionCharSwap :: State -> State
instructionCharSwap = instructionSwap char

instructionCharRot :: State -> State
instructionCharRot = instructionRot char

instructionCharFlush :: State -> State
instructionCharFlush = instructionFlush char

instructionCharEq :: State -> State
instructionCharEq = instructionEq char

instructionCharStackDepth :: State -> State
instructionCharStackDepth = instructionStackDepth char

instructionCharYank :: State -> State
instructionCharYank = instructionYank char

instructionCharYankDup :: State -> State
instructionCharYankDup = instructionYankDup char

instructionCharIsStackEmpty :: State -> State
instructionCharIsStackEmpty = instructionIsStackEmpty char

instructionCharShove :: State -> State
instructionCharShove = instructionShove char

instructionCharShoveDup :: State -> State
instructionCharShoveDup = instructionShoveDup char

instructionCharDupItems :: State -> State
instructionCharDupItems = instructionDupItems char
