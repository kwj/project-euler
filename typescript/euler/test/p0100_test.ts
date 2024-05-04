import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0100.ts";

Deno.test("normal case 1", () => {
  const actual = compute(1_000_000_000_000);
  const expected = "756872327473";
  assertEquals(actual, expected);
});
