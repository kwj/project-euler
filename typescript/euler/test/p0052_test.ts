import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0052.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "142857";
  assertEquals(actual, expected);
});
