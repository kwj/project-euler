import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0051.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "121313";
  assertEquals(actual, expected);
});
