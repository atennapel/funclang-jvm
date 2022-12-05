Experiments in compiling a functional language to the JVM.

Test it out:
```
sbt "run examples/Factorial"
java examples/Factorial
```

TODO:
- [x] IR
  - [x] IR Types
  - [x] Basic 1 parameter closures
  - [x] Remove 0-parameter methods for now
  - [x] Remove parameter names in IR
  - [x] Add if expression in IR
  - [x] Add some Int primitives in IR
  - [x] Add let expression in IR
  - [x] Basic Tail-call optimization
  - [x] Fix closures
  - [x] Box before closure application and unbox after
  - [x] Use more efficient Boxing (intValue)
- [x] Language with lambdas
  - [x] Basic syntax
  - [x] Uncurrying and lambda smashing
  - [x] Closure conversion
  - [x] Lambda lifting
  - [x] Compile to IR language
- [x] Surface language
  - [x] Basic syntax
  - [x] Type checking and elaboration
  - [x] Parser
  - [x] Special case main method for testing
  - [x] Allow pre-declaration of definitions
  - [x] Unification and holes
- [x] Polymorphism
- [ ] Algebraic data types
- [ ] IR code generation improvements
  - [x] Try to eta-expand based on type to reduce closures
  - [x] Beta-reduction
  - [ ] Inlining
  - [ ] Case commutation
  - [ ] What order do the core steps have to be done in, should they be repeated?
  - [ ] Optimize unit parameters and returns
  - [ ] Cache curried global
  - [ ] Optimize partial applications
  - [ ] Generate Function for all primitive combinations
  - [ ] Closures with arity
  - [ ] Improve TCO
