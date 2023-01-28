
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0048.ts";

Deno.test("normal case 1", () => {
  const actual = compute(10n);
  const expected = "0405071317";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(1_000n);
  const expected = "9110846700";
  assertEquals(actual, expected);
});
