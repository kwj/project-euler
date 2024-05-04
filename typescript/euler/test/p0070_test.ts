import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0070.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "8319823";
  assertEquals(actual, expected);
});
