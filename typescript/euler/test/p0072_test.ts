import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0072.ts";

Deno.test("normal case 1", () => {
  const actual = compute(8);
  const expected = "21";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(1_000_000);
  const expected = "303963552391";
  assertEquals(actual, expected);
});
