# Solutions in OCaml

## How to run

```console
sh solve.sh <problem_number> [-f data_file]
```

The following are examples.

```console
sh solve.sh 1
```
```console
sh solve.sh 22 -f data/p022_names.txt
```

## Notes

It was confirmed to work with OCaml 5.0.0.

I used the following libraries/packages in addition to the standard libraries part of OCaml 5.0.

- Str [^1]
- Zarith 1.12[^2]
- Core v0.16.0[^3]

I also used the following package for compilation.

- Dune 3.8.2[^4]

[^1]: It's distributd with OCaml itself.

[^2]: https://github.com/ocaml/Zarith

[^3]: https://github.com/janestreet/core

[^4]: https://github.com/ocaml/dune
