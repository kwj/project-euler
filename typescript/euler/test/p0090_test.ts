import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0090.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "1217";
  assertEquals(actual, expected);
});
