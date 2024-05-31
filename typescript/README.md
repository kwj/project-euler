# Solutions in TypeScript with the Deno runtime

## How to run

The [Deno](https://deno.com) runtime is required. These solutions will not work with the Node.js runtime.

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

Caution: These solutions aren't good examples for programming because I'm a JavaScript/TypeScript newbie.

These solutions were confirmed to work in the following version.

```console
$ deno --version
deno 1.44.0 (release, aarch64-unknown-linux-gnu)
v8 12.6.228.3
typescript 5.4.5
```

I used the following libraries/packages in addition to the runtime APIs part of Deno.

- [Deno Standard Library](https://jsr.io/@std)
- [Combinatorics](https://deno.land/x/combinatorics)

See `deno.json` for details.
