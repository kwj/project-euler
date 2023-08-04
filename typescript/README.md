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

Caution: These solutions aren't good examples for programming since I'm a JavaScript/TypeScript newbie.

It was confirmed to work in Deno v1.36.0.

```js
> Deno.version
{ deno: "1.36.0", v8: "11.6.189.12", typescript: "5.1.6" }
```

I used the following libraries/packages in addition to the standard runtime APIs part of Deno v1.36.0.

- Deno Standard Modules 0.197.0[^2]
- Combinatorics 1.1.2[^3]

[^1]: https://deno.land/

[^2]: https://deno.land/std@0.197.0

[^3]: https://deno.land/x/combinatorics@1.1.2
