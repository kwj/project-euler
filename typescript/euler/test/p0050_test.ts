import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0050.ts";

Deno.test("normal case 1", () => {
  const actual = compute(100);
  const expected = "41";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(500);
  const expected = "499";
  assertEquals(actual, expected);
});

Deno.test("normal case 3", () => {
  const actual = compute(1_000);
  const expected = "953";
  assertEquals(actual, expected);
});

Deno.test("normal case 4", () => {
  const actual = compute(10_000);
  const expected = "9521";
  assertEquals(actual, expected);
});

Deno.test("normal case 5", () => {
  const actual = compute(1_000_000);
  const expected = "997651";
  assertEquals(actual, expected);
});
