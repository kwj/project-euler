const asset_dir = new URL(".", import.meta.url).pathname + "../assets/";

export function assetData(filename: string) {
  const filePath = asset_dir + filename;

  try {
    Deno.statSync(filePath);
    return Deno.readFileSync(filePath);
  } catch (err) {
    if (err instanceof Deno.errors.NotFound) {
      throw new Error(`File "${filename}" is not found`);
    } else {
      console.error(err);
      throw new Error("Abend.");
    }
  }
}
