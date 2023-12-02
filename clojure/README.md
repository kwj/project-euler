# Solutions in Clojure

## How to use

### REPL

You can try out solutions in a REPL.
Please start a REPL session according to the environment you are using.

- [Clojure CLI] `clj -A:dev`
- [Leiningen] `lein repl`

The `project-euler.core/solve` function can run each solution.
The following example shows how to run the solution program for Problem 1.

```console
project-euler.core=> (solve 1)
*****
project-euler.core=>
```

### Uber-JAR

You can also create a Uber-JAR file and run it.

```console
Enter a problem number: 1
"Elapsed time: 952.566413 msecs"
*****
Enter a problem number:
```

## Notes

These solutions were confirmed to work with Clojure 1.11.1.
