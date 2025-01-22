module Instructions.CodeInstructions where

import State
import Instructions.GenericInstructions
import Instructions.IntInstructions

isBlock :: Gene -> Bool
isBlock (Block _) = True
isBlock _ = False

blockLength :: Gene -> Int
blockLength (Block xs) = length xs
blockLength _ = 1

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

codeCombine :: Gene -> Gene -> Gene
codeCombine (Block xs) (Block ys) = Block (xs <> ys)
codeCombine (Block xs) ygene = Block (xs <> [ygene])
codeCombine xgene (Block ys) = Block (xgene : ys)
codeCombine xgene ygene = Block [xgene, ygene]

instructionCodePop :: State -> State
instructionCodePop state = instructionPop state code

instructionCodeFromExec :: State -> State
instructionCodeFromExec state@(State {_exec = (e1 : es), _code = cs}) = state {_exec = es, _code = e1 : cs}
instructionCodeFromExec state = state

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
