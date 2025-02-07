module HushGP.Instructions.CodeInstructions where

import Data.List (elemIndex)
import HushGP.State
import HushGP.Instructions.GenericInstructions
import HushGP.Instructions.IntInstructions
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

-- https://faculty.hampshire.edu/lspector/push3-description.html#Type
-- CODE.CONTAINER
findContainer :: Gene -> Gene -> Gene
findContainer (Block fullA) gene
  | length fullA <= blockLength gene = Block []
  | gene `elem` fullA = Block [] -- Not allowed to be top level
  | any isBlock fullA = findContainer' (filter isBlock fullA) gene
  | otherwise = Block []
  where
    findContainer' :: [Gene] -> Gene -> Gene
    findContainer' [] _ = Block []
    findContainer' ((Block x) : xs) g = if g `elem` x then Block x else findContainer' xs g
    findContainer' _ _ = Block [] -- This should never happen
findContainer _ _ = Block []

countDiscrepancy :: Gene -> Gene -> Int
countDiscrepancy (Block xs) (Block ys) = sum [if uncurry (==) tup then 0 else 1 | tup <- zip xs ys] + abs (length xs - length ys)
countDiscrepancy xgene ygene = if xgene == ygene then 1 else 0

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
extractTailFromBlock _ = Block []

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

codeCombine :: Gene -> Gene -> Gene
codeCombine (Block xs) (Block ys) = Block (xs <> ys)
codeCombine (Block xs) ygene = Block (ygene : xs)
codeCombine xgene (Block ys) = Block (xgene : ys)
codeCombine xgene ygene = Block [xgene, ygene]

codeMember :: Gene -> Gene -> Bool
codeMember (Block _) (Block _) = False -- Can't compare two lists with `elem`
codeMember (Block xs) ygene = ygene `elem` xs
codeMember _ _ = False

codeRecursiveSize :: Gene -> Int
codeRecursiveSize (Block xs) = sum [codeRecursiveSize x + if isBlock x then 1 else 0 | x <- xs]
codeRecursiveSize _ = 1

instructionCodePop :: State -> State
instructionCodePop = instructionPop code

instructionCodeIsCodeBlock :: State -> State
instructionCodeIsCodeBlock state@(State {_code = (c : cs), _bool = bs}) = state {_code = cs, _bool = isBlock c : bs}
instructionCodeIsCodeBlock state = state

instructionCodeIsSingular :: State -> State
instructionCodeIsSingular state@(State {_code = (c : cs), _bool = bs}) = state {_code = cs, _bool = not (isBlock c) : bs}
instructionCodeIsSingular state = state

instructionCodeLength :: State -> State
instructionCodeLength state@(State {_code = (c : cs), _int = is}) = state {_code = cs, _int = blockLength c : is}
instructionCodeLength state = state

-- CODE.CAR
instructionCodeFirst :: State -> State
instructionCodeFirst state@(State {_code = (c : cs)}) = state {_code = extractFirstFromBlock c : cs}
instructionCodeFirst state = state

instructionCodeLast :: State -> State
instructionCodeLast state@(State {_code = (c : cs)}) = state {_code = extractLastFromBlock c : cs}
instructionCodeLast state = state

-- CODE.CDR
-- https://erp12.github.io/pyshgp/html/core_instructions.html#code-rest
instructionCodeTail :: State -> State
instructionCodeTail state@(State {_code = (c : cs)}) = state {_code = extractTailFromBlock c : cs}
instructionCodeTail state = state

-- |Takes the tail of a block starting at an index determined by the int stack
-- https://faculty.hampshire.edu/lspector/push3-description.html#Type
-- This is the CODE.NTHCDR command
instructionCodeTailN :: State -> State
instructionCodeTailN state@(State {_code = Block bc : cs, _int = i : is}) = state {_code = Block (drop index bc) : cs, _int = is}
  where
    index :: Int
    index = abs i `mod` length bc
instructionCodeTailN state = state

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
instructionCodeDo state@(State {_code = c1 : cs, _exec = es}) = state {_code = cs, _exec = c1 : es}
instructionCodeDo state = state

instructionCodeDoDup :: State -> State
instructionCodeDoDup state@(State {_code = (c1 : cs), _exec = es}) = state {_code = c1 : cs, _exec = c1 : es}
instructionCodeDoDup state = state

-- https://erp12.github.io/pyshgp/html/core_instructions.html#code-do-then-pop
instructionCodeDoThenPop :: State -> State
instructionCodeDoThenPop state@(State {_code = c1 : _, _exec = es}) = state {_exec = c1 : StateFunc (instructionCodePop, "instructionCodePop") : es}
instructionCodeDoThenPop state = state

codeFromExec :: Gene
codeFromExec = StateFunc (instructionCodeFromExec, "instructionCodeFromExec")

codeDoRange :: Gene
codeDoRange = StateFunc (instructionCodeDoRange, "instructionCodeDoRange")

instructionCodeDoRange :: State -> State
instructionCodeDoRange state@(State {_code = (c1 : cs), _int = (i0 : i1 : is), _exec = es}) =
  if increment i0 i1 /= 0
    then state {_exec = c1 : Block [GeneInt (i1 + increment i0 i1), GeneInt i0, codeFromExec, c1, codeDoRange] : es, _int = i1 : is, _code = cs}
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
    else state {_code = cs, _int = is, _exec = Block [GeneInt 0, GeneInt $ i - 1, codeFromExec, c, codeDoRange] : es}
instructionCodeDoCount state = state

instructionCodeDoTimes :: State -> State
instructionCodeDoTimes state@(State {_code = (c : cs), _int = (i : is), _exec = es}) =
  if i < 1
    then state
    else state {_code = cs, _int = is, _exec = Block [GeneInt 0, GeneInt $ i - 1, codeFromExec, Block [StateFunc (instructionIntPop, "instructionIntPop"), c], codeDoRange] : es}
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
instructionCodeDup = instructionDup code

instructionCodeDupN :: State -> State
instructionCodeDupN = instructionDupN code

instructionCodeSwap :: State -> State
instructionCodeSwap = instructionSwap code

instructionCodeRot :: State -> State
instructionCodeRot = instructionRot code

instructionCodeFlush :: State -> State
instructionCodeFlush = instructionFlush code

instructionCodeEq :: State -> State
instructionCodeEq = instructionEq code

instructionCodeStackDepth :: State -> State
instructionCodeStackDepth = instructionStackDepth code

instructionCodeYank :: State -> State
instructionCodeYank = instructionYank code

instructionCodeYankDup :: State -> State
instructionCodeYankDup = instructionYankDup code

instructionCodeIsStackEmpty :: State -> State
instructionCodeIsStackEmpty = instructionIsStackEmpty code

instructionCodeShove :: State -> State
instructionCodeShove = instructionShove code

instructionCodeShoveDup :: State -> State
instructionCodeShoveDup = instructionShoveDup code

instructionCodeFromBool :: State -> State
instructionCodeFromBool = instructionCodeFrom bool GeneBool 

instructionCodeFromInt :: State -> State
instructionCodeFromInt = instructionCodeFrom int GeneInt

instructionCodeFromChar :: State -> State
instructionCodeFromChar = instructionCodeFrom char GeneChar

instructionCodeFromFloat :: State -> State
instructionCodeFromFloat = instructionCodeFrom float GeneFloat

instructionCodeFromString :: State -> State
instructionCodeFromString = instructionCodeFrom string GeneString

instructionCodeFromVectorInt :: State -> State
instructionCodeFromVectorInt = instructionCodeFrom vectorInt GeneVectorInt

instructionCodeFromVectorFloat :: State -> State
instructionCodeFromVectorFloat = instructionCodeFrom vectorFloat GeneVectorFloat

instructionCodeFromVectorString :: State -> State
instructionCodeFromVectorString = instructionCodeFrom vectorString GeneVectorString

instructionCodeFromVectorBool :: State -> State
instructionCodeFromVectorBool = instructionCodeFrom vectorBool GeneVectorBool

instructionCodeFromVectorChar :: State -> State
instructionCodeFromVectorChar = instructionCodeFrom vectorChar GeneVectorChar

instructionCodeFromExec :: State -> State
instructionCodeFromExec = instructionCodeFrom exec id

instructionCodeContainer :: State -> State
instructionCodeContainer state@(State {_code = c1 : c2 : cs}) = state {_code = findContainer c1 c2 : cs}
instructionCodeContainer state = state

instructionCodeDiscrepancy :: State -> State
instructionCodeDiscrepancy state@(State {_code = c1 : c2 : cs, _int = is}) = state {_code = cs, _int = countDiscrepancy c1 c2 : is}
instructionCodeDiscrepancy state = state

instructionCodeNoOp :: State -> State
instructionCodeNoOp state = state

instructionCodeDupItems :: State -> State
instructionCodeDupItems = instructionDupItems code
