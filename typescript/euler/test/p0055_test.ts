import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0055.ts";

Deno.test("normal case 1", () => {
  const actual = compute(10_000);
  const expected = "249";
  assertEquals(actual, expected);
});
