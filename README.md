Experiments in compiling a functional language to the JVM.

Test it out:
```
"test () = 42;" > test.txt
sbt "run test.txt Test.class"
java Test
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
- [x] Language with lambdas
  - [x] Basic syntax
  - [x] Uncurrying and lambda smashing
  - [x] Closure conversion
  - [x] Lambda lifting
  - [x] Compile to IR language
- [ ] Surface language
  - [x] Basic syntax
  - [x] Type checking and elaboration
  - [x] Parser
  - [x] Special case main method for testing
  - [ ] Unification and holes
- [ ] IR code generation improvements
  - [ ] Optimize unit parameters and returns
  - [ ] Cache curried global
  - [ ] Optimize partial applications
  - [ ] Closures with arity
  - [ ] Improve TCO
