import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0041.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "7652413";
  assertEquals(actual, expected);
});
