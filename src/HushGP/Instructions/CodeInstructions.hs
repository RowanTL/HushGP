module HushGP.Instructions.CodeInstructions where

import Data.List (elemIndex)
import HushGP.State
import HushGP.Instructions.GenericInstructions
import HushGP.Instructions.IntInstructions
import HushGP.Instructions.Utility
-- import Debug.Trace

-- |Pops the top of the code stack
instructionCodePop :: State -> State
instructionCodePop = instructionPop code

-- |Checks if the top code item is a Block
instructionCodeIsCodeBlock :: State -> State
instructionCodeIsCodeBlock state@(State {_code = c1 : cs, _bool = bs}) = state {_code = cs, _bool = isBlock c1 : bs}
instructionCodeIsCodeBlock state = state

-- |Checks if the top code item is not a Block
instructionCodeIsSingular :: State -> State
instructionCodeIsSingular state@(State {_code = c1 : cs, _bool = bs}) = state {_code = cs, _bool = not (isBlock c1) : bs}
instructionCodeIsSingular state = state

-- |Checks the length of the top code item. If item is a block, counts the size, if not, returns 1
instructionCodeLength :: State -> State
instructionCodeLength state@(State {_code = c1 : cs, _int = is}) = state {_code = cs, _int = blockLength c1 : is}
instructionCodeLength state = state

-- CODE.CAR
-- |If the top item on the code stack is a Block, extracts the first item and places it onto the code stack. Acts as a NoOp otherwise.
instructionCodeFirst :: State -> State
instructionCodeFirst state@(State {_code = c1 : cs}) = state {_code = extractFirstFromBlock c1 : cs}
instructionCodeFirst state = state

-- |If the top item on the code stack is a Block, extracts the last item and places it onto the code stack. Acts as a NoOp otherwise.
instructionCodeLast :: State -> State
instructionCodeLast state@(State {_code = c1 : cs}) = state {_code = extractLastFromBlock c1 : cs}
instructionCodeLast state = state

-- |If the top item on the code stack is a Block, extracts the tail of said Block and places it onto the code stace. Acts as a NoOp otherwise.
-- CODE.CDR
-- https://erp12.github.io/pyshgp/html/core_instructions.html#code-rest
instructionCodeTail :: State -> State
instructionCodeTail state@(State {_code = c1 : cs}) = state {_code = extractTailFromBlock c1 : cs}
instructionCodeTail state = state

-- |If the top item on the code stack is a Block, takes the tail of said block starting at an index determined by the int stack
-- and pushes the result to the code stack.
-- Acts as a NoOp if not a Block.
-- https://faculty.hampshire.edu/lspector/push3-description.html#Type
-- This is the CODE.NTHCDR command
instructionCodeTailN :: State -> State
instructionCodeTailN state@(State {_code = Block bc : cs, _int = i : is}) = state {_code = Block (drop index bc) : cs, _int = is}
  where
    index :: Int
    index = abs i `mod` length bc
instructionCodeTailN state = state

-- |If the top item on the code stack is a Block, takes the init of said Block and places the result on top of the code stack.
-- Acts as a NoOp otherwise
-- https://erp12.github.io/pyshgp/html/core_instructions.html#code-but-last
instructionCodeInit :: State -> State
instructionCodeInit state@(State {_code = c1 : cs}) = state {_code = extractInitFromBlock c1 : cs}
instructionCodeInit state = state

-- |Wraps the top item in the code stack in a Block no matter the type.
instructionCodeWrap :: State -> State
instructionCodeWrap state@(State {_code = c1 : cs}) = state {_code = Block [c1] : cs}
instructionCodeWrap state = state

-- |Wraps the top two items in the code stack in a Block no matter the type.
instructionCodeList :: State -> State
instructionCodeList state@(State {_code = c1 : c2 : cs}) = state {_code = Block [c1, c2] : cs}
instructionCodeList state = state

-- |Combines the top two items on the code stack based on whether they are a block or not.
-- Check out the codeCombine utility function for how this works.
instructionCodeCombine :: State -> State
instructionCodeCombine state@(State {_code = c1 : c2 : cs}) = state {_code = codeCombine c1 c2 : cs}
instructionCodeCombine state = state

-- |Moves the top item from the code stack to the exec stack
instructionCodeDo :: State -> State
instructionCodeDo state@(State {_code = c1 : cs, _exec = es}) = state {_code = cs, _exec = c1 : es}
instructionCodeDo state = state

-- |Moves the top item from the code stack to the exec stack, doesn't delete the original item from the code stack.
instructionCodeDoDup :: State -> State
instructionCodeDoDup state@(State {_code = c1 : cs, _exec = es}) = state {_code = c1 : cs, _exec = c1 : es}
instructionCodeDoDup state = state

-- |Places the top code item onto the exec stack (doesn't delete it from the code stack), then places an instructionCodePop onto
-- the exec stack.
-- https://erp12.github.io/pyshgp/html/core_instructions.html#code-do-then-pop
instructionCodeDoThenPop :: State -> State
instructionCodeDoThenPop state@(State {_code = c1 : _, _exec = es}) = state {_exec = c1 : StateFunc (instructionCodePop, "instructionCodePop") : es}
instructionCodeDoThenPop state = state

-- |Evaluates the top item on the code stack for each step along the range i to j. Both i and j are taken from the int stack.
instructionCodeDoRange :: State -> State
instructionCodeDoRange state@(State {_code = c1 : cs, _int = i0 : i1 : is, _exec = es}) =
  if increment i0 i1 /= 0
    then state {_exec = c1 : Block [GeneInt (i1 + increment i0 i1), GeneInt i0, StateFunc (instructionCodeFromExec, "instructionCodeFromExec"), c1, StateFunc (instructionCodeDoRange, "instructionCodeDoRange")] : es, _int = i1 : is, _code = cs}
    else state {_exec = c1: es, _int = i1 : is, _code = cs}
  where
    increment :: Int -> Int -> Int
    increment destIdx currentIdx 
      | currentIdx < destIdx = 1
      | currentIdx > destIdx = -1
      | otherwise = 0
instructionCodeDoRange state = state

-- |Evaluates the top item on the code stack for each step along the range i to j. Both i and j are taken from the int stack.
instructionCodeDoCount :: State -> State
instructionCodeDoCount state@(State {_code = c : cs, _int = i1 : is, _exec = es}) =
  if i1 < 1
    then state
    else state {_code = cs, _int = is, _exec = Block [GeneInt 0, GeneInt $ i1 - 1, StateFunc (instructionCodeFromExec, "instructionCodeFromExec"), c, StateFunc (instructionCodeDoRange, "instructionCodeDoRange")] : es}
instructionCodeDoCount state = state

-- |Evaluates the top item on the code stack n times, where n comes from the n comes from the top of the int stack.
instructionCodeDoTimes :: State -> State
instructionCodeDoTimes state@(State {_code = c : cs, _int = i1 : is, _exec = es}) =
  if i1 < 1
    then state
    else state {_code = cs, _int = is, _exec = Block [GeneInt 0, GeneInt $ i1 - 1, StateFunc (instructionCodeFromExec, "instructionCodeFromExec"), Block [StateFunc (instructionIntPop, "instructionIntPop"), c], StateFunc (instructionCodeDoRange, "instructionCodeDoRange")] : es}
instructionCodeDoTimes state = state

-- |If the top boolean is true, execute the top element of the code stack and skip the second. Otherwise, skip the top element of the code stack and execute the second.
instructionCodeIf :: State -> State
instructionCodeIf state@(State {_code = c1 : c2 : cs, _bool = b1 : bs, _exec = es}) = state{_code = cs, _bool = bs, _exec = (if b1 then c1 else c2) : es}
instructionCodeIf state = state

-- |Evalutates the top code item if the top bool is true. Otherwise the top code is popped.
instructionCodeWhen :: State -> State
instructionCodeWhen state@(State {_code = c1 : cs, _bool = b1 : bs, _exec = es}) = state{_code = cs, _bool = bs, _exec = if b1 then c1 : es else es}
instructionCodeWhen state = state

-- |Pushes true to the bool stack if the second to top code item is found within the first code item. Pushes False if not.
instructionCodeMember :: State -> State
instructionCodeMember state@(State {_code = c1 : c2 : cs, _bool = bs}) = state{_code = cs, _bool = codeMember c1 c2 : bs}
instructionCodeMember state = state

-- |Pushes the nth element from a Block onto the code stack based on an index from the int stack.
-- If the top of the code stack is not a block, the int is still eaten.
-- This one doesn't count the recursive Blocks while instructionCodeExtract does
-- https://erp12.github.io/pyshgp/html/core_instructions.html#code-nth
instructionCodeN :: State -> State
instructionCodeN state@(State {_code = (Block c1) : cs, _int = i1 : is}) =
  if not $ null c1
    then state {_code = c1 !! index : cs, _int = is}
    else state
  where
    index :: Int
    index = abs i1 `mod` length c1
instructionCodeN state@(State {_code = c1 : cs, _int = _ : is}) = state {_code = c1 : cs, _int = is}
instructionCodeN state = state

-- |Makes an empty Block and pushes it to the top of the code stack.
instructionMakeEmptyCodeBlock :: State -> State
instructionMakeEmptyCodeBlock state@(State {_code = cs}) = state {_code = Block [] : cs}

-- |If the top of the code stack is a Block, pushes True to the bool stack if it is and False if it's not.
-- If the top item of the code stack is not a Block, False gets pushed to the bool stack
instructionIsEmptyCodeBlock :: State -> State
instructionIsEmptyCodeBlock state@(State {_code = Block c1 : cs, _bool = bs}) = state{_code = cs, _bool = null c1 : bs}
instructionIsEmptyCodeBlock state@(State {_code = _ : cs, _bool = bs}) = state{_code = cs, _bool = False : bs}
instructionIsEmptyCodeBlock state = state

-- |Pushes the size of the top code item to the int stack. If it's a Block, the size is counted recursively. If
-- it's not a Block, 1 gets pushed to the int stack.
instructionCodeSize :: State -> State
instructionCodeSize state@(State {_code = c1 : cs, _int = is}) = state{_code = cs, _int = codeRecursiveSize c1 : is}
instructionCodeSize state = state

-- |Pushes the size of the top code item recursively counting the nested Blocks.
-- There's a bug for this instruction in pysh where the last item in the
-- top level Block isn't counted, and if passed 0, then the entire codeblock is returned.
-- I designed this function differently so 0 returns the 0th element, and the last item
-- in the codeblock can be returned.
instructionCodeExtract :: State -> State
instructionCodeExtract state@(State {_code = block@(Block c1) : cs, _int = i1 : is}) =
  let
    index = abs i1 `mod` codeRecursiveSize block
  in
  state{_code = codeAtPoint c1 index : cs, _int = is}
instructionCodeExtract state@(State {_code = cs, _int = _ : is}) = state{_code = cs, _int = is}
instructionCodeExtract state = state

-- |Inserts a code item into a block recursively entering the nested Blocks if needed based on the top
-- int from the int stack. If the top code item isn't a Block, coerces the top item into a Block.
instructionCodeInsert :: State -> State
instructionCodeInsert state@(State {_code = block@(Block c1) : c2 : cs, _int = i1 : is}) =
  let
    index = abs i1 `mod` codeRecursiveSize block
  in
    state{_code = Block (codeInsertAtPoint c1 c2 index) : cs, _int = is}
instructionCodeInsert state@(State {_code = c1 : c2 : cs, _int = i1 : is}) =
  let
    index = abs i1 `mod` codeRecursiveSize (Block [c1])
  in
    state{_code = Block (codeInsertAtPoint [c1] c2 index) : cs, _int = is}
instructionCodeInsert state = state

-- |If the top code item is a Block that is empty, pushes 0 to the int stack if c2 is also an empty Block and -1 if not.
-- If the top code item is a Block that is not empty, pushes the index found of the second code item if found, -1 if not.
-- If neither the top code item or second code item are Blocks, checks equality. If equal, pushes 1 to int stack, pushes 0 if not.
instructionCodeFirstPosition :: State -> State
instructionCodeFirstPosition state@(State {_code = (Block []) : c2 : cs, _int = is}) = state {_code = cs, _int = (if c2 == Block [] then 0 else -1) : is}
instructionCodeFirstPosition state@(State {_code = (Block c1) : c2 : cs, _int = is}) = state {_code = cs, _int = positionElem c1 c2 : is}
  where
    positionElem :: [Gene] -> Gene -> Int
    positionElem genes gene =
      case elemIndex gene genes of
        Nothing -> -1
        Just x -> x
instructionCodeFirstPosition state@(State {_code = c1 : c2 : cs, _int = is}) = state {_code = cs, _int = (if c1 == c2 then 0 else -1) : is}
instructionCodeFirstPosition state = state

-- |If the top of the code stack is a Block, reverses the elements of the Block. Acts as a NoOp otherwise.
instructionCodeReverse :: State -> State
instructionCodeReverse state@(State {_code = (Block c1) : cs}) = state {_code = Block (reverse c1) : cs}
instructionCodeReverse state = state

-- |Duplicates the top of the code stack.
instructionCodeDup :: State -> State
instructionCodeDup = instructionDup code

-- |Duplicates the top of the code stack N times based on the top int.
instructionCodeDupN :: State -> State
instructionCodeDupN = instructionDupN code

-- |Swaps the top two code items.
instructionCodeSwap :: State -> State
instructionCodeSwap = instructionSwap code

-- |Rotates the top three code items.
instructionCodeRot :: State -> State
instructionCodeRot = instructionRot code

-- |Sets the code stack to []
instructionCodeFlush :: State -> State
instructionCodeFlush = instructionFlush code

-- |Checks if the top code items are equal. Pushes True to the bool stack if so, False if not.
instructionCodeEq :: State -> State
instructionCodeEq = instructionEq code

-- |Pushes the size of the code stack to the int stack.
instructionCodeStackDepth :: State -> State
instructionCodeStackDepth = instructionStackDepth code

-- |Moves an item from deep within the code stack to the top of the code stack based on
-- the top int from the int stack.
instructionCodeYank :: State -> State
instructionCodeYank = instructionYank code

-- |Copies an item from deep within the code stack to the top of the code stack based on
-- the top int from the int stack.
instructionCodeYankDup :: State -> State
instructionCodeYankDup = instructionYankDup code

-- |If the code stack is empty, pushes True to bool stack, else False.
instructionCodeIsStackEmpty :: State -> State
instructionCodeIsStackEmpty = instructionIsStackEmpty code

-- |Moves an item from the top of the code stack to deep within the code stack based on
-- the top int from the int stack.
instructionCodeShove :: State -> State
instructionCodeShove = instructionShove code

-- |Copies an item from the top of the code stack to deep within the code stack based on
-- the top int from the int stack.
instructionCodeShoveDup :: State -> State
instructionCodeShoveDup = instructionShoveDup code

-- |Takes the top bool from the bool stack and places said GeneBool on the code stack.
instructionCodeFromBool :: State -> State
instructionCodeFromBool = instructionCodeFrom bool GeneBool 

-- |Takes the top int from the int stack and places said GeneInt on the code stack.
instructionCodeFromInt :: State -> State
instructionCodeFromInt = instructionCodeFrom int GeneInt

-- |Takes the top char from the char stack and places said GeneChar on the code stack.
instructionCodeFromChar :: State -> State
instructionCodeFromChar = instructionCodeFrom char GeneChar

-- |Takes the top float from the float stack and places said GeneFloat on the code stack.
instructionCodeFromFloat :: State -> State
instructionCodeFromFloat = instructionCodeFrom float GeneFloat

-- |Takes the top string from the string stack and places said GeneString on the code stack.
instructionCodeFromString :: State -> State
instructionCodeFromString = instructionCodeFrom string GeneString

-- |Takes the top vectorInt from the vectorInt stack and places said GeneVectorInt on the code stack.
instructionCodeFromVectorInt :: State -> State
instructionCodeFromVectorInt = instructionCodeFrom vectorInt GeneVectorInt

-- |Takes the top vectorFloat from the vectorFloat stack and places said GeneVectorFloat on the code stack.
instructionCodeFromVectorFloat :: State -> State
instructionCodeFromVectorFloat = instructionCodeFrom vectorFloat GeneVectorFloat

-- |Takes the top vectorString from the vectorString stack and places said GeneVectorString on the code stack.
instructionCodeFromVectorString :: State -> State
instructionCodeFromVectorString = instructionCodeFrom vectorString GeneVectorString

-- |Takes the top vectorBool from the vectorBool stack and places said GeneVectorBool on the code stack.
instructionCodeFromVectorBool :: State -> State
instructionCodeFromVectorBool = instructionCodeFrom vectorBool GeneVectorBool

-- |Takes the top vectorChar from the vectorChar stack and places said GeneVectorChar on the code stack.
instructionCodeFromVectorChar :: State -> State
instructionCodeFromVectorChar = instructionCodeFrom vectorChar GeneVectorChar

-- |Takes the top gene from the exec stack and places a gene on the code stack.
instructionCodeFromExec :: State -> State
instructionCodeFromExec = instructionCodeFrom exec id

-- |Pushes the "container" of the second code stack item within 
-- the first code stack item onto the code stack. If second item contains the first 
-- anywhere (i.e. in any nested list) then the container is the smallest sub-list that 
-- contains but is not equal to the first instance. For example, if the top piece of code 
-- is "( B ( C ( A ) ) ( D ( A ) ) )" and the second piece of code is "( A )" then 
-- this pushes ( C ( A ) ). Pushes an empty list if there is no such container. 
instructionCodeContainer :: State -> State
instructionCodeContainer state@(State {_code = c1 : c2 : cs}) = state {_code = findContainer c1 c2 : cs}
instructionCodeContainer state = state

-- |Pushes a measure of the discrepancy between the top two CODE stack items onto the INTEGER stack. This will be zero if the top two items 
-- are equivalent, and will be higher the 'more different' the items are from one another. The calculation is as follows:
-- 1. Construct a list of all of the unique items in both of the lists (where uniqueness is determined by equalp). Sub-lists and atoms all count as items.
-- 2. Initialize the result to zero.
-- 3. For each unique item increment the result by the difference between the number of occurrences of the item in the two pieces of code.
-- 4. Push the result. 
instructionCodeDiscrepancy :: State -> State
instructionCodeDiscrepancy state@(State {_code = c1 : c2 : cs, _int = is}) = state {_code = cs, _int = countDiscrepancy c1 c2 : is}
instructionCodeDiscrepancy state = state

-- |Just a NoOp
instructionCodeNoOp :: State -> State
instructionCodeNoOp state = state

-- |Duplicates the top N items of the code stack based on the top of the int stack.
instructionCodeDupItems :: State -> State
instructionCodeDupItems = instructionDupItems code
