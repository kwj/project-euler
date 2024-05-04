import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0023.ts";

Deno.test("normal case 1", () => {
  const actual = compute(28123);
  const expected = "4179871";
  assertEquals(actual, expected);
});
