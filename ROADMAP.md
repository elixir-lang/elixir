## Roadmap

In the long term, here are a few things we would like to add:

### Methods and functions

* Add partial function application, function pipeline (f1 + f2) and an easy way to retrieve functions from objects (1#add and Integer##add? What about local functions?)
* Extend guards support in methods
* Add guards in functions and allow functions to have several clauses
* Add eval
* Add alias\_method, remove\_method and undef\_method
* Allow inline, public and private as decorators

### Namespaces and Refinements

* Refinements
* Data copy between modules
* Improve constant lookup (and namespaces?) (currently constants are referenced by their full name)

### Others

* Dict comprehensions and get rid of inbin and inlist
* Add more OTP behaviors: supervisors, apps, fsm and events
* method\_missing + super

### Optimizations

* Do not eval code when reading files instead, quickly compile them to a module
* Method dispatch to types specified at parse time could be optimized by skipping method lookup
* regexps could be inlined to avoid runtime compilation. Inlining can happen by using Erlang's inline directive with hidden local methods.