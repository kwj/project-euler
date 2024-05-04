import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0058.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "26241";
  assertEquals(actual, expected);
});
