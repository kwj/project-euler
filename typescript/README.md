<!-- deno-fmt-ignore-file -->
# Solutions in TypeScript with the Deno runtime

## How to run

The [Deno](https://deno.com) runtime is required. These solutions will not work at other runtimes as is.

```console
sh solve.sh <problem_number> [problem_number ...]
```

The following are examples of execution.

```console
sh solve.sh 1
```

```console
sh solve.sh 31 32 33
```

## Notes

Caution: These solutions aren't good examples for learning programming because I'm new to JavaScript/TypeScript.

These solutions were confirmed to work in the following version.

```console
$ deno --version
deno 2.3.1 (stable, release, aarch64-unknown-linux-gnu)
v8 13.5.212.10-rusty
typescript 5.8.3
```

I used the following libraries/packages in addition to the runtime APIs part of Deno.

- [Deno Standard Library](https://jsr.io/@std)
- [Combinatorics](https://deno.land/x/combinatorics)

See `deno.json` for details.
