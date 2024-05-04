import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0013.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "5537376230";
  assertEquals(actual, expected);
});
