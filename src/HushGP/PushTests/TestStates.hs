module HushGP.PushTests.TestStates where

import HushGP.State
import Data.Map qualified as Map

exampleState :: State
exampleState =
  State
    { _exec = [],
      _code = [],
      _int = [3, 5, 8, 9, 6, 10, 11, 15],
      _float = [3.23, 9.235, 5.3211, 8.0],
      _bool = [True, False],
      _string = ["abc", "123"],
      _char = ['d', 'e', 'f'],
      _parameter = [],
      _vectorInt = [[1, 2], [5, 6, 8]],
      _vectorFloat = [[1.234, 9.21], [5.42, 6.221, 8.5493]],
      _vectorBool = [[True, False], [False, False, True]],
      _vectorString = [["this is a sentence", "this is also a sentence"], ["s0", "s1", "s2"]],
      _vectorChar = [['z', 'x'], ['r', 'a', 't', 'l']],
      _input = Map.empty
    }

