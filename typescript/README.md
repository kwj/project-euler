# Solutions in TypeScript with the Deno runtime

## How to run

The Deno[^1] runtime is required. These solutions will not work with the Node.js runtime.

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

```js
> Deno.version
{ deno: "1.40.2", v8: "12.1.285.6", typescript: "5.3.3" }
```

I used the following libraries/packages in addition to the standard runtime APIs part of Deno.

- Deno Standard Modules 0.213.0[^2]
- Combinatorics 1.1.2[^3]

[^1]: https://deno.land/

[^2]: https://deno.land/std@0.213.0

[^3]: https://deno.land/x/combinatorics@1.1.2
