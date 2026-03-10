export const assetData = async (filename: string): Promise<string> => {
  const url = new URL('../assets/' + filename, import.meta.url);
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Response status: ${response.status}`);
    }
    return await response.text();
  } catch (error) {
    if (error instanceof Error) {
      throw new Error(`${error.message}: ${url.href}`);
    } else {
      throw new Error(String(error));
    }
  }
}
