# Solutions in Clojure

## How to use

You can try out solutions in a REPL.
Please start a REPL session according to the environment you are using.

- [Clojure CLI] `clj`

The `project-euler.core/solve` function can run each solution.
The following example shows how to run the solver for Problem 1 with the Clojure CLI.

```console
user=> (require '[project-euler.core :as pe])
nil
user=> (pe/solve 1)
*****
user=>
```

The `project-euler.core/-main` function may be executed to run the solver I/F.

```console
$ clj -M -m project-euler.core
Enter a problem number: 1
"Elapsed time: 55.562357 msecs"
*****
Enter a problem number:

```

In other words, you can create an Uber-JAR file.

## Notes

These solutions were confirmed to work with Clojure 1.12.0.
