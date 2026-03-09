const asset_dir = new URL(".", import.meta.url).pathname + "../assets/";

export const assetData = async (filename: string): Promise<string> => {
  const filePath = asset_dir + filename;
  return await Deno.readTextFile(filePath);
}
