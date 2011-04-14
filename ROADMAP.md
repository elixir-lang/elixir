## Roadmap

In the long term, here are a few things we would like to add:

### Methods and functions

* Add partial function application, function pipeline (f1 + f2) and an easy way to retrieve functions from objects (1#add and Integer##add? What about local functions?)
* Extend guards support in methods
* Add guards in functions and allow functions to have several clauses
* Add eval and instance_eval
* Add alias\_method, remove\_method and undef\_method
* Allow method declarations to finish with ",", meaning another method with the same name and arity must be defined next
* Make method definitions return a pointer to allow decorators
* Allow inline, public and private as decorators

### Namespaces and Refinements

* Refinements
  * To support refinements, flat modules needs to be less aggressive and allow object checkpoints
  * Method cache table (in C) will also be required if we want to allow refinements to be applied to modules
* Allow copy and/or inheritance from another object
  * The current mixin/proto system do not support inheritance
* Improve constant lookup (and namespaces?) (currently constants are referenced by their full name)

### Others

* Dict comprehensions and get rid of inbin and inlist
* 'Foo::Bar should work without requiring double quotes
* Add more OTP behaviors: supervisors, apps, fsm and events

### Optimizations

* Do not eval code when reading files instead, quickly compile them to a module
* object_mixins lookup on method dispatch can be faster and/or cached in the compiled object
* Already existent constants could be inserted directly in the abstract tree instead of looked up in runtime
* Method dispatch to types specified at parse time could be optimized by skipping method lookup
* regexps could be inlined to avoid runtime compilation. Inlining can happen by storing it in the module as an attribute or by using Erlang's inline directive with hidden local methods.