Experiments in compiling a functional language to the JVM.

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
  - [ ] Unit IR type (?)
  - [ ] Optimize unit parameters and returns
  - [ ] Cache curried global
  - [ ] Optimize partial applications
  - [ ] Closures with arity
  - [ ] Improve TCO
- [ ] Language with lambdas
  - [ ] Closure conversion
  - [ ] Lambda lifting
  - [ ] Compile to IR language
