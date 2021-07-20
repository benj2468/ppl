# Week 4 Assignment (Interpreter)

The core component of this new implementation is a `Scope` class. The `Scope` class does two things of importance:

1. First loops through all of it's lines, and saves all of it's function definitions
2. Then loops through all the lines similar to before with a program counter (though with the new implementation of scopes this isn't necessary)

# Loops and Functions

> How loops work, and how function-calls work

## Loops

A `Loop` is an extensions of a `Scope`. The loop has lines, similar to a `Scope`, a series of functions that are in scope (the same as it's parent scope, and then whatever is provided by it's individual scope), and memory, which starts out the same as it's parent scope, and then it's parent scope is updated with the resulting memory (because we consider loops to share memory scope as their parent).

The difference between a `Loop` and a `Scope` is that the `Loop`'s lines get run over and over again as long as the `Loop`'s condition is satisfied.

## Functions

A `Function` is an extension of a `Scope`. It has lines, a memory, and functions similar to a `Scope`. The functions that are provided on initialization to a `Function` are that of the parent `Scope`, the memory is initialized as empty, accept for any variables passed into the `Function`. Any changes made in a `Function` are applies to the `Function`'s scope. Then at the end of the function, the `Function`'s memory that is associated with the argument is returned to the parent `Scope`'s memory.

# Nested Loops

> Whether nested loops are supported and why

Nested Loops should be supported, because a loop's body is simply another `Scope`, so adding a new loop in that `Scope` should not be a problem at all.

# Nested Functions

> Whether nested functions are supported, why, and how to test this

Nested Functions should be supported, because a function's body is simply another `Scope`, so adding a new function in that `Scope` should not be a problem at all.

# Recursive Functions

> Whether recursive functions are supported and why

Recursive Functions should work, because we share a function's parent `Scope`'s functions with the new Function. Thus, `Function` `call_x` will always have `call_x` as a `Function` in it's scope.

# Author

- Benjamin Cape '22
