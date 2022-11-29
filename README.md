Experiments in compiling a functional language to the JVM.

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
- [ ] Language with lambdas
  - [ ] Closure conversion
  - [ ] Lambda lifting
  - [ ] Compile to IR language
- [ ] IR code generation improvements
  - [ ] Optimize unit parameters and returns
  - [ ] Cache curried global
  - [ ] Optimize partial applications
  - [ ] Closures with arity
  - [ ] Improve TCO
