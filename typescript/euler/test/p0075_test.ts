import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0075.ts";

Deno.test("normal case 1", () => {
  const actual = compute(48);
  const expected = "6";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(1_500_000);
  const expected = "161667";
  assertEquals(actual, expected);
});
