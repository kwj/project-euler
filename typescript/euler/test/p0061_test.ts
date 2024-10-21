import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0061.ts";

Deno.test("normal case 1", () => {
  const actual = compute(4);
  const expected = "8181";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(5);
  const expected = "19291";
  assertEquals(actual, expected);
});

Deno.test("normal case 3", () => {
  const actual = compute(8);
  const expected = "28684";
  assertEquals(actual, expected);
});
