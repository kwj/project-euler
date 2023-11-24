# Solutions in Julia

## How to run

```console
sh solve.sh
```

The following are examples of execution.

```console
julia> solve_0001()

julia> @time solve_0002()
```

You can run the test suite in the Pkg REPL.

```console
(Euler) pkg> test
```

## Notes

It was confirmed to work with Julia v1.9.4.

I used the following packages and their dependent packages.

- Combinatorics v1.0.2
- DataStructures v0.18.15
- LinearAlgebra[^1]
- Primes v0.5.5

[^1]: It's distributd with Julia v1.9.4.
