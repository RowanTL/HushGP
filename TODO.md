# TODO

## Push Language TODO

- [X] Make all vector functions applicable to string functions and vice versa
- [X] Implement all functions as seen in propeller
- [X] Implement all functions as seen in the specification
- [ ] Implement Linear Algebra functions as specified in the previous papers
  - [ ] These are in a separate branch, just need merging now
- [X] Add a function to sort a vector forward and backwards
- [X] Disambiguate isEmpty and stackIsEmpty
- [X] Rename Logical to Bool
- [X] Make int yank, shove, yankdup, and shovedup generic
- [X] Write haddock documentation for each function
- [X] Refactor all functions to take state as the final parameter
- [X] Standardize the pattern matching parameter names, such as c1 : cs
- [ ] Write unit/quickcheck tests for the generic functions
- [X] Use template haskell to generate function lists
- [X] Move utility functions to their own file
- [ ] Make add/sub/mult/div/mod instructions generic
- [ ] Use template haskell to (mostly) generate functions from generic ones (Split files based on the arity of their functions)
- [ ] Add more special functions like sqrt, pow

## PushGP TODO
- [X] Implement a Plushy genome translator
  - [X] Implement ~~silent and~~ skip marker(s) as well
  ~~[ ] Have close amt of 1,2, and 3~~
  - [X] Need a random genome generator
    - I'm only going to implement propeller's :specified version
      - Is the best according to the papers
  - [X] Need a NoOp that opens blocks
  - [ ] Have a way to balance amount of closes with open blocks
- [ ] Need to make genomes serializable (Check pysh json files)
- [ ] Add Memory
- [ ] Add history stack(s), like a call stack
- [ ] Implement interpreter options (PushArgs would work well)
  - Should probably place this in a separate file
- [X] Implement different forms of downsampling
- [ ] Implement concurrent execution of creating random plushies and evaluating individuals
- [X] Devise a good way to implement ERCs
- [ ] Implement random simplification of genomes
  - [ ] Find a way to multi-thread this
- [ ] Look at using `uniformShuffleList` over System.Random.Shuffle
