# TODO

## Push Language TODO

- [X] Make all vector functions applicable to string functions and vice versa
- [X] Implement all functions as seen in propeller
- [X] Implement all functions as seen in the specification
- [ ] Implement Linear Algebra functions as specified in the previous papers
- [X] Add a function to sort a vector forward and backwards
- [X] Disambiguate isEmpty and stackIsEmpty
- [X] Rename Logical to Bool
- [X] Make int yank, shove, yankdup, and shovedup generic
- [X] Write haddock documentation for each function
- [X] Refactor all functions to take state as the final parameter
- [X] Standardize the pattern matching parameter names, such as c1 : cs
- [ ] Write unit/quickcheck tests for all of the instructions
~~[ ] Use template haskell to generate function lists~~
- [X] Move utility functions to their own file
- [ ] Make add/sub/mult/div/mod instructions generic

## PushGP TODO
- [ ] Implement a Plushy genome translator
  - [ ] Need to make this reproducable too (Check pysh json files)
- [ ] Add Memory
- [ ] Add history stack(s), like a call stack
- [ ] Implement interpreter options (could probably just place this into a map)
