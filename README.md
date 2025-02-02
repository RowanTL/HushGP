# Notes about this branch

This branch is abandoned. I'm going to switch my efforts to learning [tasty](https://hackage.haskell.org/package/tasty).

# HushGP
A PushGP implementation in Haskell

This branch is meant to to go one by one and finish implementing all of
the functions in the pyshgp list.

https://erp12.github.io/pyshgp/html/core_instructions.html

# Big Problem

There is no easy way to determine equality of two functions in Haskell. No comparing names, no nothing.
We coult compare applying two functions to an example state, but that would get tedious and costly quickly.

The only idea floating in my head at the moment is to attach a string to the `StateFunc` Gene to
track what the functions are. This would require a painful redefinition of the tests, but I think would be
worth it in the grand scheme. Would mean we could also track the functions when outputting them after training.

## Tasks
* [ ] Post minimal core of exec to haskell discourse for advice about speed optimization.
* [x] Do test-driven development on this one.
    * [x] Write tests for every function.
    * [x] tests/ are just copied from make-grade, need to write for this project.
    * [ ] Included examples of basic assertions, QuickCheck, Hspec, hspec-quickcheck.
* [x] Look at Lenses library for abstraction

## Design considerations
The biggest design constraint is that for the exec stack (but not data stacks)
we have to be able to detect type at runtime.

A simple way to do this for the exec stack is as a list of custom data type.
That custom Gene data type must have as many sub-types as there are types + fuction types.

If the input stack is singular, then it needs a general Gene data type,
but if there was an input stack per type, they could be specific.

I would really like to benchmark some of the following three versions for speed: 

1) Where some functions can act on all stacks (this repo),
and thus every data stack is a list of a more general Gene type,
elements of which are wrapped in data TypeGene so they can be identified in stack-general functions.
To bind all the stacks for convenience,
we could put each stack list in a tuple, or a custom data type, Data.Map or Data.HashMap.
The exec stack will always need a more general Gene type,
with Gene types wrapping each individual thing, for runtime identification.

2) Where type-specific functions act on each stack independently, 
and thus each data stack can have exclusive specific basic types,
which are not wrapped in data TypeGene, because they do not need to be identified.
To bind all the stacks for convenience,
we could put each stack list in a tuple, or a custom data type,
but not in a or Data.Map or Data.HashMap, as those require homogenous (K, V) pairs.
The exec stack will always need a more general Gene type,
with Gene types wrapping each individual thing, for runtime identification.

3) Alternatively, for the exec stack, we could store strings, 
and eval strings (similar to my custumized version of propel clojure)
Regular and input stacks can stil be either TypeGene or basic types.
This is clearly not ideal.

4) For the exec stack itself,
typeable, data generic, ghc.generic, data.dynamic, heterogeneous lists, etc. could also help,
to detect the type of variables at runtime, but I would rather stick to language basics at first.
