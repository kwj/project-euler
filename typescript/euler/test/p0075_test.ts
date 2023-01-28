
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0075.ts";

Deno.test("normal case 1", () => {
  const actual = compute(1_500_000);
  const expected = "161667";
  assertEquals(actual, expected);
});
