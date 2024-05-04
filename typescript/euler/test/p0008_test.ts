import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0008.ts";

Deno.test("normal case 1", () => {
  const actual = compute(4);
  const expected = "5832";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(13);
  const expected = "23514624000";
  assertEquals(actual, expected);
});
