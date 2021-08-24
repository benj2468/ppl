# Week 9 Assignment

For this weeks Assignment I have written 3 different attribute macros that provide different functionality for function calls.

1. `time_it`
2. `in_prime_field`
3. `cacher`

## Testing

Testing is run by running `cargo test`. If you don't have `cargo` installed, simply run `brew install rustup` and that should do the trick. The tests can be seen in [`week9/test/test.rs`](./test/test.rs)

## Now for descriptions of each macro

### `time_it`

This macro is incredibly simple, by applying it to a function, you will get runtimes logged in your terminal. If you want them logged to a file, simply specify a file name as a parameter to the macro, and it will log to that file instead.

## `in_prime_field`

This macro performs all arithmetic operations within the provided function within the field specified. If the field is not of prime order, the program will panic and crash - so make sure it's prime :). One caveat with this, since we are using code-generation before rust's compile time we cannot perform the modulo operation on function calls and their return values. Thus, there is another provided attribute here `field` that can be applied to assignment statements such as in the test, that tell the `is_prime_field` macro to make that return value modulo `p`.

## `cacher`

Cacher is incredible simple, it saves the inputs and outputs in a cache (JSON file), so that it doesn't have to recalculate the values each time. It also saves a hashed value of the entire token stream of the function, to make sure that if you chance the function, then we don't still use the same cache. BUT, if you chance a function that this function uses, we won't be able to know, so make sure you delete the cache.

# Author

Benjamin Cape '22
