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

It was confirmed to work with Deno v1.30.3.

```js
> Deno.version
{ deno: "1.30.3", v8: "10.9.194.5", typescript: "4.9.4" }
```

I used the following libraries/packages in addition to the standard runtime APIs part of Deno v1.30.3.

- Deno Standard Modules 0.177.0[^2]
- Combinatorics 1.1.2[^3]

If you try to run these solutions on newer version of Deno, you should update import map for Deno Standard Modules in the configuration file.
You can check [versions.json](https://github.com/denoland/dotland/blob/main/versions.json) for the corresponding version of Deno Standard Modules.

[^1]: https://deno.land/

[^2]: https://deno.land/std@0.177.0

[^3]: https://deno.land/x/combinatorics@1.1.2
