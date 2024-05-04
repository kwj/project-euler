import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0047.ts";

Deno.test("normal case 1", () => {
  const actual = compute(4);
  const expected = "134043";
  assertEquals(actual, expected);
});
