
// This file is the entry point.

if (Deno.args.length > 0) {
  if (Deno.args.every((val) => !isNaN(Number(val)))) {
    const solutions_dir = "bin";

    for (const elem of Deno.args) {
      const num = elem.padStart(4, "0");
      const filename = `p${num}.ts`;

      console.log(`[Problem ${elem}]`);
      await import(`./${solutions_dir}/${filename}`)
        .then((module) => {
          if ("solve" in module) {
            module.solve();
          } else {
            console.log(`Entry point solve() isn't exist in '${filename}'.`);
          }
        })
        .catch((err) => {
          console.log(err);
        });
      console.log("");
    }
  } else {
    const arg = Deno.args.find((val) => isNaN(Number(val)));
    console.log(`Syntax error:\n  Invalid argument: ${arg}`);
  }
} else {
  console.log("Syntax error:\n  no arguments");
}
