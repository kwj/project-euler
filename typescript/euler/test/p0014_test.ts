import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0014.ts";

Deno.test("normal case 1", () => {
  const actual = compute(1_000_000);
  const expected = "837799";
  assertEquals(actual, expected);
});
