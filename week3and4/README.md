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

A `Function` is an extension of a `Scope`. It has lines, a memory, and functions similar to a `Scope`. The functions that are provided on initialization to a `Function` are that of the parent `Scope` and itself, the memory is initialized as empty, except for any variables passed into the `Function`. Any changes(memory changes) made in a `Function` are applied to the `Function`'s scope. Then at the end of the function, the `Function`'s memory that is associated with the argument is returned to the parent `Scope`'s memory. Unlike other scopes, when we initialize a `Function` we do not initialize it's scope, we only save it's lines. We only create a new scope when we call the function.

# Nested Loops

> Whether nested loops are supported and why

Nested Loops should be supported, because a loop's body is simply another `Scope`, so adding a new loop in that `Scope` should not be a problem at all. An illustration of this being tested can be found in my [test.py0](./test.py0#37) file.

# Nested Functions

> Whether nested functions are supported, why, and how to test this

Nested Functions should be supported, because a function's body is simply another `Scope`, so adding a new function in that `Scope` should not be a problem at all. An illustration of this being tested can be found in my [test.py0](./test.py0#L1) file.

# Recursive Functions

> Whether recursive functions are supported and why

Recursive Functions should work, because we share a function's parent `Scope`'s functions with the new Function. Thus, `Function` `call_x` will always have `call_x` as a `Function` in it's scope. A test of this functionality can be found in [test_recursive.py0](./test_recursive.py0). Since we don't have base cases, or other arithmetic easily accessible, the ability for this to work properly is illustrated by the debugging output of this program, and that it runs forever and doesn't fail when trying to call `foo()` again.

# Testing

I've added some testing functionality. By typing out what I expect to be printed on each run, I can check using my `make test` command that the output is as expected for each file.

# Some extra notes

- Note that in line 39 of `exampleProgram-1.py0`, `path` is not defined yet. As a result, nothing is printed. Rather than erroring, I think of `path`, or any uninstantialized variable as containing no content, thus it succeeds that running, but prints nothing.
- I've added the ability to store data of any tuple size. See [test.py](./test.py0#L57) for examples.
  - With the previous change, I decided to take the liberty of allowing the variable names i.e. 1,2,3 etc. used when extracting values to either be in scope or not be in scope. This allows the following:
  ```
  PRINT[1,3] = foo[1,2,3]
  ```
  So if the programmer wants to only print the first and third values, then they can do so without running into an error. This is seen also in [test.py](./test.py0#L67)
  - Also with this change, you can enter into the same variable name, tuples of different lengths. This runs along the same vein as python where lists can contains whatever types you want, and are not restrictive. As a result, printing a value that does not exist in one tuple will simply output an "\_" (underscore). THis can also be seen in the output of [test.py](./test_output.txt#L9-L11)

# Version

All code must be run on python3.8

# Author

- Benjamin Cape '22
