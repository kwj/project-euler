import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0046.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "5777";
  assertEquals(actual, expected);
});
