import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0078.ts";

Deno.test("normal case 1", () => {
  const actual = compute(1_000_000);
  const expected = "55374";
  assertEquals(actual, expected);
});
