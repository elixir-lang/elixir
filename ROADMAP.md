## Roadmap

In the long term, here are a few things we would like to add:

* Add partial function application, function pipeline (f1 + f2) and an easy way to retrieve functions from objects (1#add and Integer##add?)
* Extend guards support in methods
* Add guards in functions and allow funtions to have several clauses
* Allow object definitions to be reopened or add refinements
* Allow copy and/or inheritance from another object
* Improve constant lookup (currently constants are referenced by their full name)
* Add eval and instance_eval
* Add alias\_method, remove\_method and undef\_method
* Allow extension of builtin types (like inheriting from Integer)
* Do not eval code when reading files
* Allow method declarations to finish with ",", meaning another method with the same name and arity must be defined next
* Make method definitions return a pointer to the defined method to allow further manipulation

And a few performance optimizations:

* object_mixins lookup on method dispatch can be faster
* Already existent constants could be inserted directly in the abstract tree instead of looked up in runtime
* Method dispatch to types specified at parse time could be optimized by skipping method lookup