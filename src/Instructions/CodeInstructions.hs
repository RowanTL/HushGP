module Instructions.CodeInstructions where

import Data.List (elemIndex)
import State
import Instructions.GenericInstructions
import Instructions.IntInstructions
import Control.Lens

-- import Debug.Trace

isBlock :: Gene -> Bool
isBlock (Block _) = True
isBlock _ = False

blockLength :: Gene -> Int
blockLength (Block xs) = length xs
blockLength _ = 1

blockIsNull :: Gene -> Bool
blockIsNull (Block xs) = null xs
blockIsNull _ = False

-- I think I can abstract the boilerplate base case check for a lot of these
-- with a different function

-- empty Blocks are a thing but that shouldn't really matter
extractFirstFromBlock :: Gene -> Gene
extractFirstFromBlock (Block (x : _)) = x
extractFirstFromBlock gene = gene

extractLastFromBlock :: Gene -> Gene
extractLastFromBlock (Block []) = Block []
extractLastFromBlock (Block xs) = last xs
extractLastFromBlock gene = gene

extractInitFromBlock :: Gene -> Gene
extractInitFromBlock (Block []) = Block []
extractInitFromBlock (Block xs) = Block (init xs)
extractInitFromBlock gene = gene

extractTailFromBlock :: Gene -> Gene
extractTailFromBlock (Block xs) = Block (drop 1 xs)
extractTailFromBlock gene = gene

-- This function took at least 3 hours to program.
codeAtPoint :: [Gene] -> Int -> Gene
codeAtPoint (gene : _) 0 = gene
codeAtPoint [] _ = Block [] -- Should only happen if an empty block is last Gene in the list of Genes
codeAtPoint ((Block nestedGenes) : genes) index = codeAtPoint (nestedGenes <> genes) (index - 1)
codeAtPoint (_ : genes) index = codeAtPoint genes (index - 1)

codeInsertAtPoint :: [Gene] -> Gene -> Int -> [Gene]
codeInsertAtPoint oldGenes gene 0 = gene : oldGenes
codeInsertAtPoint [] gene _ = [gene] -- This shouldn't happen (lol)
codeInsertAtPoint ((Block genes) : oldGenes) gene index = Block (codeInsertAtPoint genes gene (index - 1)) : oldGenes
codeInsertAtPoint (oldGene : oldGenes) gene index = oldGene : codeInsertAtPoint oldGenes gene (index - 1) 

-- This one functions differently than pysh.
-- I like this one because it preserves ordering in the second case
codeCombine :: Gene -> Gene -> Gene
codeCombine (Block xs) (Block ys) = Block (xs <> ys)
codeCombine (Block xs) ygene = Block (xs <> [ygene])
codeCombine xgene (Block ys) = Block (xgene : ys)
codeCombine xgene ygene = Block [xgene, ygene]

codeMember :: Gene -> Gene -> Bool
codeMember (Block _) (Block _) = False -- Can't compare two lists with `elem`
codeMember (Block xs) ygene = ygene `elem` xs
codeMember _ _ = False

-- I love list comprehensions
codeRecursiveSize :: Gene -> Int
codeRecursiveSize (Block xs) = sum [codeRecursiveSize x + if isBlock x then 1 else 0 | x <- xs]
codeRecursiveSize _ = 1

instructionCodePop :: State -> State
instructionCodePop state = instructionPop state code

-- instructionCodeFromExec :: State -> State
-- instructionCodeFromExec state@(State {_exec = (e1 : es), _code = cs}) = state {_exec = es, _code = e1 : cs}
-- instructionCodeFromExec state = state

instructionCodeIsCodeBlock :: State -> State
instructionCodeIsCodeBlock state@(State {_code = (c : cs), _bool = bs}) = state {_code = cs, _bool = isBlock c : bs}
instructionCodeIsCodeBlock state = state

instructionCodeIsSingular :: State -> State
instructionCodeIsSingular state@(State {_code = (c : cs), _bool = bs}) = state {_code = cs, _bool = not (isBlock c) : bs}
instructionCodeIsSingular state = state

instructionCodeLength :: State -> State
instructionCodeLength state@(State {_code = (c : cs), _int = is}) = state {_code = cs, _int = blockLength c : is}
instructionCodeLength state = state

instructionCodeFirst :: State -> State
instructionCodeFirst state@(State {_code = (c : cs)}) = state {_code = extractFirstFromBlock c : cs}
instructionCodeFirst state = state

instructionCodeLast :: State -> State
instructionCodeLast state@(State {_code = (c : cs)}) = state {_code = extractLastFromBlock c : cs}
instructionCodeLast state = state

-- https://erp12.github.io/pyshgp/html/core_instructions.html#code-rest
instructionCodeTail :: State -> State
instructionCodeTail state@(State {_code = (c : cs)}) = state {_code = extractTailFromBlock c : cs}
instructionCodeTail state = state

-- https://erp12.github.io/pyshgp/html/core_instructions.html#code-but-last
instructionCodeInit :: State -> State
instructionCodeInit state@(State {_code = (c : cs)}) = state {_code = extractInitFromBlock c : cs}
instructionCodeInit state = state

instructionCodeWrap :: State -> State
instructionCodeWrap state@(State {_code = (c : cs)}) = state {_code = Block [c] : cs}
instructionCodeWrap state = state

instructionCodeList :: State -> State
instructionCodeList state@(State {_code = (c1 : c2 : cs)}) = state {_code = Block [c1, c2] : cs}
instructionCodeList state = state

instructionCodeCombine :: State -> State
instructionCodeCombine state@(State {_code = (c1 : c2 : cs)}) = state {_code = codeCombine c1 c2 : cs}
instructionCodeCombine state = state

instructionCodeDo :: State -> State
instructionCodeDo state@(State {_code = (c1 : cs), _exec = es}) = state {_code = cs, _exec = c1: es}
instructionCodeDo state = state

instructionCodeDoDup :: State -> State
instructionCodeDoDup state@(State {_code = (c1 : cs), _exec = es}) = state {_code = c1 : cs, _exec = c1 : es}
instructionCodeDoDup state = state

-- https://erp12.github.io/pyshgp/html/core_instructions.html#code-do-then-pop
instructionCodeDoThenPop :: State -> State
instructionCodeDoThenPop state@(State {_code = c1 : _, _exec = es}) = state {_exec = c1 : StateFunc instructionCodePop : es}
instructionCodeDoThenPop state = state

instructionCodeDoRange :: State -> State
instructionCodeDoRange state@(State {_code = (c1 : cs), _int = (i0 : i1 : is), _exec = es}) =
  if increment i0 i1 /= 0
    then state {_exec = c1 : Block [GeneInt (i1 + increment i0 i1), GeneInt i0, StateFunc instructionCodeFromExec, c1, StateFunc instructionCodeDoRange] : es, _int = i1 : is, _code = cs}
    else state {_exec = c1: es, _int = i1 : is, _code = cs}
  where
    increment :: Int -> Int -> Int
    increment destIdx currentIdx 
      | currentIdx < destIdx = 1
      | currentIdx > destIdx = -1
      | otherwise = 0
instructionCodeDoRange state = state

instructionCodeDoCount :: State -> State
instructionCodeDoCount state@(State {_code = (c : cs), _int = (i : is), _exec = es}) =
  if i < 1
    then state
    else state {_code = cs, _int = is, _exec = Block [GeneInt 0, GeneInt $ i - 1, StateFunc instructionCodeFromExec, c, StateFunc instructionCodeDoRange] : es}
instructionCodeDoCount state = state

instructionCodeDoTimes :: State -> State
instructionCodeDoTimes state@(State {_code = (c : cs), _int = (i : is), _exec = es}) =
  if i < 1
    then state
    else state {_code = cs, _int = is, _exec = Block [GeneInt 0, GeneInt $ i - 1, StateFunc instructionCodeFromExec, Block [StateFunc instructionIntPop, c], StateFunc instructionCodeDoRange] : es}
instructionCodeDoTimes state = state

instructionCodeIf :: State -> State
instructionCodeIf state@(State {_code = (c1 : c2 : cs), _bool = (b1 : bs), _exec = es}) = state{_code = cs, _bool = bs, _exec = (if b1 then c1 else c2) : es}
instructionCodeIf state = state

instructionCodeWhen :: State -> State
instructionCodeWhen state@(State {_code = (c1 : cs), _bool = (b1 : bs), _exec = es}) = state{_code = cs, _bool = bs, _exec = if b1 then c1 : es else es}
instructionCodeWhen state = state

instructionCodeMember :: State -> State
instructionCodeMember state@(State {_code = (c1 : c2 : cs), _bool = bs}) = state{_code = cs, _bool = codeMember c1 c2 : bs}
instructionCodeMember state = state

-- This one doesn't count the recursive Blocks while instructionCodeExtract does
-- https://erp12.github.io/pyshgp/html/core_instructions.html#code-nth
instructionCodeN :: State -> State
instructionCodeN state@(State {_code = ((Block c1) : cs), _int = (i1 : is)}) =
  if not $ null c1
    then state {_code = c1 !! index : cs, _int = is}
    else state
  where
    index :: Int
    index = abs i1 `mod` length c1
instructionCodeN state@(State {_code = (c1 : cs), _int = _ : is}) = state {_code = c1 : cs, _int = is}
instructionCodeN state = state

instructionMakeEmptyCodeBlock :: State -> State
instructionMakeEmptyCodeBlock state@(State {_code = cs}) = state {_code = Block [] : cs}

instructionIsEmptyCodeBlock :: State -> State
instructionIsEmptyCodeBlock state@(State {_code = Block c1 : cs, _bool = bs}) = state{_code = cs, _bool = null c1 : bs}
instructionIsEmptyCodeBlock state@(State {_bool = bs}) = state{_bool = False : bs}

instructionCodeSize :: State -> State
instructionCodeSize state@(State {_code = c1 : cs, _int = is}) = state{_code = cs, _int = codeRecursiveSize c1 : is}
instructionCodeSize state = state

-- There's a bug for this instruction in pysh where the last item in the
-- top level Block isn't counted, and if passed 0, then the entire codeblock is returned.
-- I designed this function differently so 0 returns the 0th element, and the last item
-- in the codeblock can be returned.
instructionCodeExtract :: State -> State
instructionCodeExtract state@(State {_code = (block@(Block c1) : cs), _int = i1 : is}) =
  let
    index = abs i1 `mod` codeRecursiveSize block
  in
  state{_code = codeAtPoint c1 index : cs, _int = is}
instructionCodeExtract state@(State {_code = cs, _int = _ : is}) = state{_code = cs, _int = is}
instructionCodeExtract state = state

instructionCodeInsert :: State -> State
instructionCodeInsert state@(State {_code = (block@(Block c1) : c2 : cs), _int = i1 : is}) =
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

-- How do I test if two functions are the same??????????
-- This will impact the final case. This implementation can't determine
-- if two functions are the same, so it assumes that they are.
-- Maybe can test for equality by seeing if these two functions affect the current state
-- in the same way.
instructionCodeFirstPosition :: State -> State
instructionCodeFirstPosition state@(State {_code = (Block []) : c2 : cs, _int = is}) = state {_code = cs, _int = (if c2 == Block [] then 0 else -1) : is}
instructionCodeFirstPosition state@(State {_code = (Block c1) : c2 : cs, _int = is}) = state {_code = cs, _int = positionElem c1 c2 : is}
  where
    -- This is really not gonna be good for StateFunc
    positionElem :: [Gene] -> Gene -> Int
    positionElem genes gene =
      case elemIndex gene genes of
        Nothing -> -1
        Just x -> x
instructionCodeFirstPosition state@(State {_code = c1 : c2 : cs, _int = is}) = state {_code = cs, _int = (if c1 == c2 then 0 else -1) : is}
instructionCodeFirstPosition state = state

instructionCodeReverse :: State -> State
instructionCodeReverse state@(State {_code = (Block c1) : cs}) = state {_code = Block (reverse c1) : cs}
instructionCodeReverse state = state

instructionCodeDup :: State -> State
instructionCodeDup state = instructionDup state code

instructionCodeDupN :: State -> State
instructionCodeDupN state = instructionDupN state code

instructionCodeSwap :: State -> State
instructionCodeSwap state = instructionSwap state code

instructionCodeRot :: State -> State
instructionCodeRot state = instructionRot state code

instructionCodeFlush :: State -> State
instructionCodeFlush state = instructionFlush state code

instructionCodeEq :: State -> State
instructionCodeEq state = instructionEq state code

instructionCodeStackDepth :: State -> State
instructionCodeStackDepth state = instructionStackDepth state code

instructionCodeYank :: State -> State
instructionCodeYank state = instructionYank state code

instructionCodeYankDup :: State -> State
instructionCodeYankDup state = instructionYankDup state code

instructionCodeStackIsEmpty :: State -> State
instructionCodeStackIsEmpty state = instructionIsEmpty state code

instructionCodeShove :: State -> State
instructionCodeShove state = instructionShove state code

instructionCodeShoveDup :: State -> State
instructionCodeShoveDup state = instructionShoveDup state code

instructionCodeFromBool :: State -> State
instructionCodeFromBool state = instructionCodeFrom state bool GeneBool 

instructionCodeFromInt :: State -> State
instructionCodeFromInt state = instructionCodeFrom state int GeneInt

instructionCodeFromChar :: State -> State
instructionCodeFromChar state = instructionCodeFrom state char GeneChar

instructionCodeFromFloat :: State -> State
instructionCodeFromFloat state = instructionCodeFrom state float GeneFloat

instructionCodeFromString :: State -> State
instructionCodeFromString state = instructionCodeFrom state string GeneString

instructionCodeFromVectorInt :: State -> State
instructionCodeFromVectorInt state = instructionCodeFrom state vectorInt GeneVectorInt

instructionCodeFromVectorFloat :: State -> State
instructionCodeFromVectorFloat state = instructionCodeFrom state vectorFloat GeneVectorFloat

instructionCodeFromVectorString :: State -> State
instructionCodeFromVectorString state = instructionCodeFrom state vectorString GeneVectorString

instructionCodeFromVectorBool :: State -> State
instructionCodeFromVectorBool state = instructionCodeFrom state vectorBool GeneVectorBool

instructionCodeFromVectorChar :: State -> State
instructionCodeFromVectorChar state = instructionCodeFrom state vectorChar GeneVectorChar

instructionCodeFromExec :: State -> State
instructionCodeFromExec state = instructionCodeFrom state exec id
