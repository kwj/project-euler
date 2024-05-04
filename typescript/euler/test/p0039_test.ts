import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0039.ts";

Deno.test("normal case 1", () => {
  const actual = compute(1_000);
  const expected = "840";
  assertEquals(actual, expected);
});
