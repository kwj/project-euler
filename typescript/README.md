# Solutions in TypeScript with the Deno runtime

## How to run

The Deno[^1] runtime is required.

```console
sh solve.sh <problem_number> [problem_number ...]
```

The following are examples.

```console
sh solve.sh 1
```

```console
sh solve.sh 31 32 33
```

## Notes

Caution: These solutions aren't good examples for programming since I am a JavaScript/TypeScript newbie.

It was confirmed to work with Deno v1.33.0.

```js
> Deno.version
{ deno: "1.33.0", v8: "11.4.183.1", typescript: "5.0.3" }
```

I used the following libraries/packages in addition to the standard runtime APIs part of Deno v1.33.0.

- Deno Standard Modules 0.185.0[^2]
- Combinatorics 1.1.2[^3]

If you try to run these solutions on newer version of Deno, you should update import map for Deno Standard Modules in the configuration file.
You can check [versions.json](https://github.com/denoland/dotland/blob/main/versions.json) for the corresponding version of Deno Standard Modules.

[^1]: https://deno.land/

[^2]: https://deno.land/std@0.185.0

[^3]: https://deno.land/x/combinatorics@1.1.2
