Experiments in compiling a functional language to the JVM.

Test it out:
```
sbt "run examples/Factorial"
java examples/Factorial
```

TODO:
- [ ] IR
  - [x] IR Types
  - [x] Basic 1 parameter closures
  - [x] Remove 0-parameter methods for now
  - [x] Remove parameter names in IR
  - [x] Add if expression in IR
  - [x] Add some Int primitives in IR
  - [x] Add let expression in IR
  - [x] Basic Tail-call optimization
  - [ ] Box before closure application and unbox after
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
- [ ] Algebraic data types
- [ ] Polymorphism
- [ ] IR code generation improvements
  - [ ] Optimize unit parameters and returns
  - [ ] Cache curried global
  - [ ] Optimize partial applications
  - [ ] Try to eta-expand based on type to reduce closures
  - [ ] Generate Function for all primitive combinations
  - [ ] Closures with arity
  - [ ] Improve TCO