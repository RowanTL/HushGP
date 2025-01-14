import Control.Exception (assert)
import GP
import Push

main :: IO ()
main = do
  let exampleGenome = [intAdd, intAdd]
  let exampleState =
        State
          { exec = [IntGene 5, FloatGene 3.4, BoolGene True, StringGene "hi"],
            int = [IntGene 1, IntGene 2, IntGene 3],
            float = [FloatGene 1.2, FloatGene 1.7],
            bool = [BoolGene True, BoolGene False],
            string = [StringGene "Hello", StringGene "Push"],
            input = [Input $ IntGene 1, Input $ StringGene "Hi", Input $ BoolGene True, Input $ FloatGene 1.3]
          }
  -- This is an example of applying one function (head exampleGenome produces intAdd) to the exampleState:
  assert ([3, 3] == map unpackIntGene (int (head exampleGenome exampleState))) pure ()
  -- This function applies an entire genome to the starting state, and produces the final state:
  assert ([6] == map unpackIntGene (int (interpretGenome exampleState exampleGenome))) pure ()
