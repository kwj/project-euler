import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0038.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "932718654";
  assertEquals(actual, expected);
});
